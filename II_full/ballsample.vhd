-- COPYRIGHT (c) 2021 ALL RIGHT RESERVED
-- Chair for Security Engineering
-- Georg Land (georg.land@rub.de)
-- License: see LICENSE file

-- THIS CODE AND INFORMATION ARE PROVIDED "AS IS" WITHOUT WARRANTY OF ANY 
-- KIND, EITHER EXPRESSED OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE
-- IMPLIED WARRANTIES OF MERCHANTABILITY AND/OR FITNESS FOR A
-- PARTICULAR PURPOSE.

library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;

library work;
use work.dilithium.all;
use work.interfaces.all;
use work.memmap.all;

entity ballsample is
    Port (
        clk : in std_logic;
        d   : in ballsample_in_type;
        q   : out ballsample_out_type
    );
end ballsample;

architecture Behavioral of ballsample is

    type state_type is (idle, rst_keccak, absorb, padding_start, padding_mid, padding_end, zeropad, zeropad_squeeze, permute, permute_squeeze, squeeze_sign_low, squeeze_sign_high, squeeze, sample, shuffle, insert, write);
    signal state, nextstate : state_type;
    
    signal shacntd : counter_in_type;
    signal shacntq : counter_out_type;
    signal shacnt : natural range 0 to SHAKE128_RATE/32-1;
    
    constant SIGNBITS_HIGH : natural := DILITHIUM_tau-32;
    constant SIGNBITS_HIGH_BYTES : natural := (DILITHIUM_tau-32)/8;
    constant SIGNBITS_HIGH_REM : natural := (DILITHIUM_tau-32) mod 8;
    signal signbits : std_logic_vector(DILITHIUM_tau-1 downto 0);
    signal signbits_load_low, signbits_load_high, signbits_shift, signbit_out : std_logic;

    signal nonzcntd : counter_in_type;
    signal nonzcntq : counter_out_type;
    signal posreg_en : std_logic;
    signal posreg_depth : natural range 0 to DILITHIUM_tau := 0;
    signal posreg_in, posreg_out : std_logic_vector(8 downto 0);
    
    signal taucntd : counter_in_type;
    signal taucntq : counter_out_type;
    signal taucnt : natural range 0 to DILITHIUM_tau-1;
    signal rejection_threshold : natural range 256-DILITHIUM_tau to 255;
    
    signal samplecntd : counter_in_type;
    signal samplecntq : counter_out_type;
    signal samplecnt : natural range 0 to 3;
    signal current_sample : std_logic_vector(7 downto 0);
    
    signal rotcntd : counter_in_type;
    signal rotcntq : counter_out_type;
    signal rotcnt : natural range 0 to DILITHIUM_tau-1;
    
    signal memcntd : counter_in_type;
    signal memcntq : counter_out_type;
    signal memcnt : natural range 0 to DILITHIUM_N/4-1;
    
    signal wen : std_logic_vector(0 to 3);
    signal waddr : std_logic_vector(5 downto 0);
    signal wsign : std_logic;
begin

-- memory stuff
memory_counter: entity work.counter
generic map (max_value => DILITHIUM_N/4-1)
port map (
    clk => clk,
    d => memcntd,
    q => memcntq,
    value => memcnt
);
process(state, memcntd, memcntq, memcnt, wen, waddr, wsign)
constant M : natural := memory_map.c.memory_index;
variable addrrev : coef_addr_array(0 to 3);
begin
    q.memd <= (others => ZEROMEM);
    if state /= write
    then
        q.memd(M).wsel <= memory_map.c.poly_index;
        q.memd(M).wen <= (others => memcntd.en and not memcntq.ovf);
        q.memd(M).waddr <= (std_logic_vector(to_unsigned(memcnt, 6)) & "00", 
        std_logic_vector(to_unsigned(memcnt, 6)) & "01", 
        std_logic_vector(to_unsigned(memcnt, 6)) & "10", 
        std_logic_vector(to_unsigned(memcnt, 6)) & "11");
        q.memd(M).wdata <= (others => (others => '0'));
    else
        q.memd(M).wsel <= memory_map.c.poly_index;
        q.memd(M).wen <= wen;
        addrrev := (waddr & "00", waddr & "01", waddr & "10", waddr & "11");
        for i in 0 to 3
        loop
            for j in 0 to 7
            loop
                q.memd(M).waddr(i)(j) <= addrrev(i)(7-j);
            end loop;
        end loop;
        if wsign = '0'
        then
            q.memd(M).wdata <= (others => std_logic_vector(to_unsigned(1, 23)));
        else
            q.memd(M).wdata <= (others => std_logic_vector(to_unsigned(DILITHIUM_Q-1, 23)));
        end if;
    end if;
end process;


sample_counter: entity work.counter
generic map (max_value => 3)
port map (
    clk => clk,
    d => samplecntd,
    q => samplecntq,
    value => samplecnt
);
current_sample <= d.keccakq.data(31-samplecnt*8 downto 32-samplecnt*8-8);

tau_counter: entity work.counter
generic map (max_value => DILITHIUM_tau-1)
port map (
    clk => clk,
    d => taucntd,
    q => taucntq,
    value => taucnt
);
rejection_threshold <= 256-DILITHIUM_tau+taucnt;

nonzero_counter: entity work.counter
generic map (max_value => DILITHIUM_tau)
port map (
    clk => clk,
    d => nonzcntd,
    q => nonzcntq,
    value => posreg_depth
);

posreg: entity work.dyn_shift_reg
generic map (width => 9, max_depth => DILITHIUM_tau)
port map (
    clk => clk,
    ce => posreg_en,
    depth => posreg_depth,
    d => posreg_in,
    q => posreg_out
);

rotation_counter: entity work.counter
generic map (max_value => DILITHIUM_tau-1)
port map (
    clk => clk,
    d => rotcntd,
    q => rotcntq,
    value => rotcnt
);

signreg: process(clk)
begin
    if rising_edge(clk)
    then
        if signbits_load_low = '1'
        then
            for i in 0 to 3
            loop
                signbits(31-i*8 downto 32-i*8-8) <= d.keccakq.data(i*8+7 downto i*8);
            end loop;
        elsif signbits_load_high = '1'
        then
            for i in 0 to SIGNBITS_HIGH_BYTES-1
            loop
                signbits(31+i*8+8 downto 32+i*8) <= d.keccakq.data(31-i*8 downto 32-i*8-8);
            end loop;
            signbits(signbits'high downto 32+SIGNBITS_HIGH_BYTES*8) <= d.keccakq.data(31-SIGNBITS_HIGH_BYTES*8-8+SIGNBITS_HIGH_REM downto 32-SIGNBITS_HIGH_BYTES*8-8);
        elsif signbits_shift = '1'
        then
            signbits(signbits'high-1 downto 0) <= signbits(signbits'high downto 1);
            signbits(signbits'high) <= '0';
        end if;
    end if;
end process;
signbit_out <= signbits(0);

sha_counter: entity work.counter
generic map (max_value => SHAKE128_RATE/32-1)
port map (
    clk => clk,
    d => shacntd,
    q => shacntq,
    value => shacnt
);

with state
select
    q.keccakd.data <=   d.seedregq.data when absorb,
                        x"1f000000" when padding_start,
                        x"00000080" when padding_end,
                        d.keccakq.data when others;

states: process(clk)
begin
    if rising_edge(clk)
    then
        if d.rst = '1'
        then
            state <= idle;
        else
            state <= nextstate;
        end if;
    end if;
end process;

signals: process(state, d, shacntq, shacnt, samplecntq, nonzcntq, current_sample, signbit_out, posreg_out, posreg_depth, rotcntq, taucntq, rotcnt, rejection_threshold)
begin

    nextstate <= state;
    
    q.ready <= '0';
    
    -- counter
    shacntd.en <= '0';
    shacntd.rst <= '0';
    samplecntd.en <= '0';
    samplecntd.rst <= '0';
    taucntd.en <= '0';
    taucntd.rst <= '0';
    nonzcntd.en <= '0';
    nonzcntd.rst <= '0';
    rotcntd.en <= '0';
    rotcntd.rst <= '0';
    memcntd.en <= '1'; -- exception since this enables zeroizing the c memory in the background
    memcntd.rst <= '0';
    
    -- keccak
    q.keccakd.en <= '0';
    q.keccakd.rst <= '0';
    
    -- seed register
    q.seedregd.en_rotate <= '0';
    q.seedregd.en_write <= '0';
    q.seedregd.data <= (others => '0');
    
    -- sign bits register
    signbits_load_low <= '0';
    signbits_load_high <= '0';
    signbits_shift <= '0';
    
    -- one and minus one position registers
    posreg_en <= '0';
    posreg_in <= (others => '0');
    
    wen <= "0000";
    waddr <= (others => '0');
    wsign <= '0';
    
    
    case state is
        when idle => 
            q.ready <= '1';
            
            shacntd.rst <= '1';
            samplecntd.rst <= '1';
            taucntd.rst <= '1';
            nonzcntd.rst <= '1';
            rotcntd.rst <= '1';
            memcntd.rst <= '1';
            memcntd.en <= '0'; -- disable zeroizing c here
            
            q.keccakd.rst <= '1';
            
            if d.en = '1'
            then
                nextstate <= rst_keccak;
            end if;
        
        when rst_keccak => 
            q.keccakd.en <= '1';
            nextstate <= absorb;
            
        when absorb =>
            shacntd.en <= '1';
            q.keccakd.en <= '1';
            q.seedregd.en_rotate <= '1';
            
            if shacnt = 256/32-1
            then
                nextstate <= padding_start;
            end if;
        
        when padding_start =>
            shacntd.en <= '1';
            q.keccakd.en <= '1';
            nextstate <= padding_mid;
            
        when padding_mid =>
            shacntd.en <= '1';
            q.keccakd.en <= '1';
            
            if shacnt = SHAKE256_RATE/32-2
            then
                nextstate <= padding_end;
            end if;
            
        when padding_end =>
            shacntd.en <= '1';
            q.keccakd.en <= '1';
            nextstate <= zeropad;
            
        when zeropad =>
            shacntd.en <= '1';
            q.keccakd.en <= '1';
            
            if d.keccakq.ready = '0' -- permutation starts
            then
                nextstate <= permute;
            end if;
            
        when zeropad_squeeze =>
            shacntd.en <= '1';
            q.keccakd.en <= '1';
            
            if d.keccakq.ready = '0' -- permutation starts
            then
                nextstate <= permute_squeeze;
            end if;
            
        when permute =>
            shacntd.rst <= '1';
            
            if d.keccakq.ready = '1' -- permutation finished
            then
                nextstate <= squeeze_sign_low;
            end if;
            
        when permute_squeeze =>
            shacntd.rst <= '1';
            
            if d.keccakq.ready = '1' -- permutation finished
            then
                nextstate <= squeeze;
            end if;
            
        when squeeze_sign_low =>
            shacntd.en <= '1';
            q.keccakd.en <= '1';
            signbits_load_low <= '1';
            nextstate <= squeeze_sign_high;
            
        when squeeze_sign_high =>
            shacntd.en <= '1';
            q.keccakd.en <= '1';
            signbits_load_high <= '1';
            nextstate <= sample;
            
        when squeeze =>
            samplecntd.rst <= '1'; -- prepare sampling
            shacntd.en <= '1';
            q.keccakd.en <= '1';
            nextstate <= sample;
        
        when sample =>
            samplecntd.en <= '1';
            rotcntd.rst <= '1'; -- prepare for shuffle
            if taucntq.ovf = '1'
            then
                nextstate <= write;
            elsif unsigned(current_sample) <= rejection_threshold
            then
                samplecntd.en <= '0';
                nextstate <= shuffle;
            elsif samplecntq.ovf = '1'
            then
                if shacnt = SHAKE256_RATE/32-1
                then
                    nextstate <= zeropad_squeeze;
                else
                    nextstate <= squeeze;
                end if;
            end if;
        
        when shuffle =>
            rotcntd.en <= '1';
            posreg_en <= '1';
            posreg_in <= posreg_out;
            if posreg_out(7 downto 0) = current_sample
            then
--                report "already in";
                posreg_in(7 downto 0) <= std_logic_vector(to_unsigned(rejection_threshold, 8));
            end if;
            
            if rotcnt >= posreg_depth-1
            then
                nonzcntd.en <= '1';
                nextstate <= insert;
            end if;
         
        when insert =>
            posreg_en <= '1';
            posreg_in(8) <= signbit_out;
            posreg_in(7 downto 0) <= current_sample;
            
            taucntd.en <= '1';
            signbits_shift <= '1';
            if samplecntq.max = '1'
            then
                if shacnt = SHAKE256_RATE/32-1
                then
                    nextstate <= zeropad_squeeze;
                else
                    nextstate <= squeeze;
                end if;
            else
                samplecntd.en <= '1';
                nextstate <= sample;
            end if;
        
        when write =>
            posreg_en <= '1';
            rotcntd.en <= '1';
            waddr <= posreg_out(7 downto 2);
            wsign <= posreg_out(8);
            case posreg_out(1 downto 0) is
                when "00" => wen <= "1000";
                when "01" => wen <= "0100";
                when "10" => wen <= "0010";
                when "11" => wen <= "0001";
                when others => wen <= "0000";
            end case;
            
            if rotcntq.max = '1'
            then
                nextstate <= idle;
            end if;
    end case;

end process;

end Behavioral;
