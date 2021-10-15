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

entity use_hint is
    Port (
        clk : in std_logic;
        d   : in use_hint_in_type;
        q   : out use_hint_out_type
    );
end use_hint;

architecture Behavioral of use_hint is

type state_type is (idle, absorb_mu, loadhint, hint, absorb, zeropad, zeropad_squeeze, padding_start, padding_finish, permute, permute_squeeze, squeeze, result_good, result_bad);
signal state, nextstate : state_type;

signal abscntd, abskeccntd : counter_in_type;
signal abscntq, abskeccntq : counter_out_type;
signal abscnt : natural range 0 to SHAKE256_RATE/32-1;

signal memcntd : counter_in_type;
signal memcntq : counter_out_type;
signal memcnt : natural range 0 to DILITHIUM_k*DILITHIUM_N/4-1;

signal omegacntd, omegakcntd : counter_in_type;
signal omegacntq, omegakcntq : counter_out_type;
signal omegacnt : natural range 0 to DILITHIUM_omega-1;
signal omegakcnt : natural range 0 to DILITHIUM_k;

signal lcntd : counter_in_type;
signal lcntq : counter_out_type;

signal hintlutd, hintlutq0, hintlutq1 : payload_array(0 to 3);
signal hintreg, hintreg_set : std_logic_vector(0 to 3) := "0000";
signal hintreg_clr : std_logic;

signal ren : std_logic;

signal absorbreg : std_logic_vector(95 downto 0);
signal absreg_en_load, absreg_en_keccak : std_logic;
signal absreg_in : std_logic_vector(DILITHIUM_highbits_len*4-1 downto 0);
signal absreg_out : std_logic_vector(31 downto 0);

type pipeline_type is record
    memcnt : natural range 0 to DILITHIUM_N/4-1;
    ren : std_logic;
end record pipeline_type;
type pipeline_array is array(1 to GLOBAL_MEMORY_DELAY+1) of pipeline_type;
signal pipeline : pipeline_array;
signal pip_in, pip_out : pipeline_type;

begin

mux: process(state, d, memcnt, ren)
variable m,p : natural;
variable addr : coef_addr_array(0 to 3);
begin
    q.memd <= (others => ZEROMEM);
    
    m := memory_map.w(memcnt / (DILITHIUM_N/4)).memory_index;
    p := memory_map.w(memcnt / (DILITHIUM_N/4)).poly_index;
    
    q.memd(m).rsel <= p;
    q.memd(m).ren <= (others => ren);
    for i in 0 to 3
    loop
        addr(i) := std_logic_vector(to_unsigned(memcnt mod (DILITHIUM_N/4), 6)) & std_logic_vector(to_unsigned(i, 2));
        for j in 0 to 7
        loop
            q.memd(m).raddr(i)(j) <= addr(i)(7-j);
        end loop;
    end loop;
    hintlutd <= d.memq(m);
end process;

absregingen: for i in 0 to 3
generate
    absreg_in((i+1)*DILITHIUM_highbits_len-1 downto i*DILITHIUM_highbits_len) 
        <= hintlutq0(i)(DILITHIUM_highbits_len-1 downto 0) when hintreg(i) = '0' else 
           hintlutq1(i)(DILITHIUM_highbits_len-1 downto 0); 
end generate;

absreg32: if DILITHIUM_gamma2 = (DILITHIUM_Q-1)/32
generate
    load_counter: entity work.counter
    generic map (max_value => 1)
    port map (
        clk => clk,
        d => lcntd,
        q => lcntq
    );
    absorb_register: process(clk)
    begin
        if rising_edge(clk)
        then
            if absreg_en_load = '1'
            then
                absorbreg(15 downto 8) <= absreg_in(7 downto 0);
                absorbreg(7 downto 0) <= absreg_in(15 downto 8);
                absorbreg(31 downto 16) <= absorbreg(15 downto 0);
            end if;
        end if;
    end process;
    absreg_out <= absorbreg(31 downto 0);
end generate;
absreg96: if DILITHIUM_gamma2 = (DILITHIUM_Q-1)/88
generate
    load_counter: entity work.counter
    generic map (max_value => 3)
    port map (
        clk => clk,
        d => lcntd,
        q => lcntq
    );
    keccak_counter: entity work.counter
    generic map (max_value => 2)
    port map (
        clk => clk,
        d => abskeccntd,
        q => abskeccntq
    );
    absorb_register: process(clk)
    begin
        if rising_edge(clk)
        then
            if absreg_en_load = '1'
            then
                absorbreg(23 downto 24-8) <= absreg_in(7 downto 0);
                absorbreg(23-8 downto 24-16) <= absreg_in(15 downto 8);
                absorbreg(23-16 downto 24-24) <= absreg_in(23 downto 16);
                absorbreg(95 downto 24) <= absorbreg(71 downto 0);
            elsif absreg_en_keccak = '1'
            then
                absorbreg(31 downto 0) <= (others => '0');
                absorbreg(95 downto 32) <= absorbreg(63 downto 0);
            end if;
        end if;
    end process;
    absreg_out <= absorbreg(95 downto 64);
end generate;

pip: process(clk)
begin
    if rising_edge(clk)
    then
        for i in GLOBAL_MEMORY_DELAY+1 downto 2
        loop
            pipeline(i) <= pipeline(i-1);
        end loop;
        pipeline(1) <= pip_in;
    end if;
end process;
pip_out <= pipeline(GLOBAL_MEMORY_DELAY+1);
pip_in.memcnt <= memcnt;
pip_in.ren <= ren;

hint_register: process(clk)
begin
    if rising_edge(clk)
    then
        if hintreg_clr = '1'
        then
            hintreg <= "0000";
        elsif hintreg_set /= "0000"
        then
            hintreg <= hintreg or hintreg_set;
        end if;
    end if;
end process;

hintlutgen: for i in 0 to 3
generate
    hintlut: entity work.use_hint_lut
    port map (
        clk => clk,
        d => hintlutd(i),
        q0 => hintlutq0(i),
        q1 => hintlutq1(i)
    );
end generate;

memory_counter: entity work.counter
generic map (max_value => DILITHIUM_k*DILITHIUM_N/4-1)
port map (
    clk => clk,
    d => memcntd,
    q => memcntq,
    value => memcnt
);

omega_counter: entity work.counter
generic map (max_value => DILITHIUM_omega-1)
port map (
    clk => clk,
    d => omegacntd,
    q => omegacntq,
    value => omegacnt
);

omega_k_counter: entity work.counter
generic map (max_value => DILITHIUM_k)
port map (
    clk => clk,
    d => omegakcntd,
    q => omegakcntq,
    value => omegakcnt
);

absorb_counter: entity work.counter
generic map (max_value => SHAKE256_RATE/32-1)
port map (
    clk => clk,
    d => abscntd,
    q => abscntq,
    value => abscnt
);

kmux: process(state, d, abscnt, abscntq, absreg_out)
begin
    q.keccakd.data <= d.keccakq.data;
    case state is
        when absorb_mu => q.keccakd.data <= d.muregq.data;
        when absorb => q.keccakd.data <= d.keccakq.data xor absreg_out;
        when padding_start => 
            if abscntq.max = '1'
            then
                q.keccakd.data <= x"1f000080" xor d.keccakq.data;
            else
                q.keccakd.data <= x"1f000000" xor d.keccakq.data;
            end if;
        when padding_finish =>
            if abscntq.max = '1'
            then
                q.keccakd.data <= x"00000080" xor d.keccakq.data;
            end if;
        when others =>
    end case;
end process;

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

q.chashregd.data <= d.keccakq.data;

signals: process(state, d, abscntq, abscnt, memcntq, memcnt, pip_out, lcntq, abskeccntq, omegacntq, omegacnt, omegakcntq, omegakcnt)
begin
    nextstate <= state;
    
    q.ready <= '0';
    q.valid <= '0';
    q.result <= '0';

    q.keccakd.en <= '0';
    q.keccakd.rst <= '0';
    
    q.muregd.en_rotate <= '0';
    q.muregd.en_write <= '0';
    q.muregd.data <= (others => '0');
    
    q.hregd.en_rotate_data <= '0';
    q.hregd.en_rotate_poly <= '0';
    q.hregd.en_write_data <= '0';
    q.hregd.en_write_poly <= '0';
    q.hregd.data_offset <= (others => '0');
    q.hregd.poly_offset <= (others => '0');
    q.chashregd.en_rotate <= '0';
    q.chashregd.en_write <= '0';
    
    hintreg_clr <= '0';
    hintreg_set <= "0000";
    
    lcntd.en <= '0';
    lcntd.rst <= '0';
    memcntd.en <= '0';
    memcntd.rst <= '0';
    abscntd.en <= '0';
    abscntd.rst <= '0';
    abskeccntd.en <= '0';
    abskeccntd.rst <= '0';
    omegacntd.en <= '0';
    omegacntd.rst <= '0';
    omegakcntd.en <= '0';
    omegakcntd.rst <= '0';
    
    absreg_en_load <= '0';
    absreg_en_keccak <= '0';
    
    ren <= '0';
    
    case state is
        when idle =>
            q.ready <= '1';
            
            lcntd.rst <= '1';
            memcntd.rst <= '1';
            abscntd.rst <= '1';
            abskeccntd.rst <= '1';
            omegacntd.rst <= '1';
            omegakcntd.rst <= '1';
            
            q.keccakd.rst <= '1';
            
            if d.en = '1' and d.keccakq.ready = '1'
            then
                q.keccakd.en <= '1';
                q.keccakd.rst <= '0';
                nextstate <= absorb_mu;
            end if;
        
        when absorb_mu =>
            abscntd.en <= '1';
            q.keccakd.en <= '1';
            q.muregd.en_rotate <= '1';
            
            if abscnt = 512/32-1
            then
                hintreg_clr <= '1';
                nextstate <= loadhint;
            end if;
        
        when loadhint =>
            ren <= '1';
            if (memcnt mod (DILITHIUM_N/4)) /= to_integer(unsigned(d.hregq.data_offset(7 downto 2)))
            or (memcnt / (DILITHIUM_N/4)) /= omegakcnt 
            or omegakcntq.ovf = '1' -- all hints read
            or d.nohint_writechash = '1' -- no hints, just compute highbits
            then
                nextstate <= hint;
            else
                q.hregd.en_rotate_data <= '1';
                omegacntd.en <= '1';
                if omegacnt >= to_integer(unsigned(d.hregq.poly_offset))-1
                then
                    q.hregd.en_rotate_poly <= '1';
                    omegakcntd.en <= '1';
                end if;
                case d.hregq.data_offset(1 downto 0) is
                    when "00" => hintreg_set <= "1000";
                    when "01" => hintreg_set <= "0100";
                    when "10" => hintreg_set <= "0010";
                    when others => hintreg_set <= "0001";
                end case;
            end if;
        
        when hint => 
            abskeccntd.rst <= '1';
            if pip_out.ren = '1' and memcnt = pip_out.memcnt -- wait until the desired value is here
            then
                absreg_en_load <= '1';
                lcntd.en <= '1';
                memcntd.en <= '1';
                if lcntq.max = '1'
                then
                    nextstate <= absorb;
                else
                    hintreg_clr <= '1';
                    nextstate <= loadhint;
                end if;
            end if;
        
        when absorb =>
            absreg_en_keccak <= '1';
            q.keccakd.en <= '1';
            abscntd.en <= '1';
            abskeccntd.en <= '1';
            lcntd.rst <= '1';
            if abscntq.max = '1'
            then
                nextstate <= zeropad;
            elsif DILITHIUM_highbits_len = 4 or abskeccntq.max = '1'
            then
                hintreg_clr <= '1';
                nextstate <= loadhint;
                if memcntq.ovf = '1'
                then
                    nextstate <= padding_start;
                end if;
            end if;
        
        when zeropad =>
            q.keccakd.en <= '1';
            if d.keccakq.ready = '0'
            then
                nextstate <= permute;
            end if;
        
        when permute =>
            q.keccakd.en <= '0';
            abscntd.rst <= '1';
            if d.keccakq.ready = '1'
            then
                if DILITHIUM_highbits_len = 4
                then
                    if memcntq.ovf = '0'
                    then
                        hintreg_clr <= '1';
                        nextstate <= loadhint;
                    else
                        nextstate <= padding_start;
                    end if;
                else
                    if abskeccntq.ovf = '0'
                    then
                        nextstate <= absorb;
                    else
                        if memcntq.ovf = '0'
                        then
                            hintreg_clr <= '1';
                            nextstate <= loadhint;
                        else
                            nextstate <= padding_start;
                        end if;
                    end if;
                end if;
            end if;
            
        when padding_start =>
            q.keccakd.en <= '1';
            abscntd.en <= '1';
            nextstate <= padding_finish;
            if abscntq.max = '1'
            then
                nextstate <= zeropad_squeeze;
            end if;
            
        when padding_finish =>
            q.keccakd.en <= '1';
            abscntd.en <= '1';
            if abscntq.max = '1'
            then
                nextstate <= zeropad_squeeze;
            end if;
        
        when zeropad_squeeze =>
            q.keccakd.en <= '1';
            if d.keccakq.ready = '0'
            then
                nextstate <= permute_squeeze;
            end if;
        
        when permute_squeeze =>
            q.keccakd.en <= '0';
            abscntd.rst <= '1';
            if d.keccakq.ready = '1'
            then
                nextstate <= squeeze;
            end if;
        
        when squeeze => 
            q.keccakd.en <= '1';
            abscntd.en <= '1';
            q.chashregd.en_rotate <= '1';
            q.chashregd.en_write <= d.nohint_writechash;
            
            if abscnt = 256/32-1
            then
                nextstate <= result_good;
            end if;
            if d.keccakq.data /= d.chashregq.data and d.nohint_writechash = '0'
            then
                nextstate <= result_bad;
            end if;
            
        when result_good =>
            q.ready <= '1';
            q.valid <= '1';
            q.result <= '1';
            
        when result_bad =>
            q.ready <= '1';
            q.valid <= '1';
            q.result <= '0';
            
    end case;
end process;

end Behavioral;
