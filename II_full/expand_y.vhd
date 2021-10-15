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
use ieee.math_real.all;

library work;
use work.dilithium.all;
use work.interfaces.all;
use work.memmap.all;

entity expand_y is
    Port (
        clk : in std_logic;
        d   : in expand_y_in_type;
        q   : out expand_y_out_type
    );
end expand_y;

architecture Behavioral of expand_y is

    constant SREG_WIDTH : natural := 288 - (DILITHIUM_loggamma1/19)*128; -- 288 for gamma=2**17, 160 for 2**19

    type state_type is (idle, absorb, permute, squeeze, sample, zeropad, reset_absorb);
    signal state, nextstate : state_type := idle;
    
    signal sample_tmp : std_logic_vector((DILITHIUM_loggamma1+1)*4-1 downto 0);
    signal current_sample, current_sample_converted : payload_array(0 to 3);
    signal sampleconv_en : std_logic;
    
    -- address tmp
    signal addr, addr_bitrev : coef_addr_array(0 to 3);
    
    -- l counter
    signal lcntd : counter_in_type;
    signal lcntq : counter_out_type;
    signal lcnt : natural range 0 to DILITHIUM_l-1;
    
    -- absorb counter
    signal abscntd : counter_in_type;
    signal abscntq : counter_out_type;
    signal abscnt : natural range 0 to SHAKE256_RATE/32;
    
    -- memory count
    signal memcntd : counter_in_type;
    signal memcntq : counter_out_type;
    signal memcnt : natural range 0 to DILITHIUM_N/4-1;
    signal memcnt_delayed : std_logic_vector(7 downto 0); -- 6 is en, 7 is ovf
    
    -- squeeze counter
    constant SQZCNT_MAX : natural := (SHAKE256_RATE/32)-1;
    signal sqzcntd : counter_in_type;
    signal sqzcntq : counter_out_type;
    signal sqzcnt : natural range 0 to SQZCNT_MAX := 0;
    
    -- keccak
    signal keccak_din : std_logic_vector(31 downto 0);
    
    -- memory
    signal mem : std_logic;

begin


-- l counter
lcounter: entity work.counter
generic map (max_value => DILITHIUM_l-1)
port map (
    clk => clk,
    d => lcntd,
    q => lcntq,
    value => lcnt
);


-- sample data fifo
q.fifoyzdatad <= d.keccakq.data;
sample_tmp <= d.fifoyzdataq;
sd: for i in 0 to 3
generate
    current_sample(i)(22 downto DILITHIUM_loggamma1+1) <= (others => '0');
    current_sample(i)(DILITHIUM_loggamma1 downto 0) <= sample_tmp((i+1)*(DILITHIUM_loggamma1+1)-1 downto i*(DILITHIUM_loggamma1+1));
end generate;


q.convertyzd.en <= sampleconv_en;
q.convertyzd.sub <= std_logic_vector(to_unsigned(2**DILITHIUM_loggamma1, 23));
q.convertyzd.data <= current_sample;
current_sample_converted <= d.convertyzq;


-- memory counter: offset for the coefficients that go into memory
memory_counter: entity work.counter
generic map (max_value => DILITHIUM_N/4-1)
port map (
    clk => clk,
    d => memcntd,
    q => memcntq,
    value => memcnt    
);



absorb_counter: entity work.counter
generic map (max_value => SHAKE128_RATE/32)
port map (
    clk => clk,
    d => abscntd,
    q => abscntq,
    value => abscnt
);

absorbmux: process(d.seedregq, d.kappa, lcnt, abscnt)
begin
    case abscnt is
        when 0 =>
            q.seedregd.en_rotate <= '0';
            q.seedregd.en_write <= '0';
            keccak_din <= (others => '0');
        when 1 to 16 =>
            q.seedregd.en_rotate <= '1';
            q.seedregd.en_write <= '0';
            keccak_din <= d.seedregq.data;
        when 17 =>
            q.seedregd.en_rotate <= '0';
            q.seedregd.en_write <= '0';
            keccak_din(31 downto 24) <= std_logic_vector(to_unsigned(d.kappa + lcnt, 8));
            keccak_din(23 downto  0) <= x"001f00";
        when 34 => -- shake 256
            q.seedregd.en_rotate <= '0';
            q.seedregd.en_write <= '0';
            keccak_din <= x"00000080";
        when others =>
            q.seedregd.en_rotate <= '0';
            q.seedregd.en_write <= '0';
            keccak_din <= (others => '0');
    end case;
end process;






-- squeeze counter
squeeze_counter: entity work.counter
generic map (max_value => SQZCNT_MAX)
port map (
    clk => clk,
    d => sqzcntd,
    q => sqzcntq,
    value => sqzcnt
);

-- keccak
q.keccakd.data <= keccak_din when abscntd.en = '1' else d.keccakq.data;






addr(0)(7 downto 2) <= memcnt_delayed(5 downto 0);
addr(1)(7 downto 2) <= memcnt_delayed(5 downto 0);
addr(2)(7 downto 2) <= memcnt_delayed(5 downto 0);
addr(3)(7 downto 2) <= memcnt_delayed(5 downto 0);
addr(0)(1 downto 0) <= "00";
addr(1)(1 downto 0) <= "01";
addr(2)(1 downto 0) <= "10";
addr(3)(1 downto 0) <= "11";

addrgen: for i in 0 to 3
generate
    bitrevgen: for b in 0 to 7
    generate
        addr_bitrev(i)(b) <= addr(i)(7-b);
    end generate;
end generate;

delay_memcnt: entity work.dyn_shift_reg
generic map (width => 8, max_depth => DELAY_CONV_YZ)
port map (
    clk => clk,
    depth => DELAY_CONV_YZ,
    ce => sampleconv_en,
    d(5 downto 0) => std_logic_vector(to_unsigned(memcnt, 6)),
    d(6) => memcntd.en,
    d(7) => memcntq.ovf,
    q => memcnt_delayed
);
memmux: process(addr_bitrev, lcnt, mem, current_sample_converted, memcnt_delayed)
begin
    for m in 0 to NUM_MEM_8_POLY-1
    loop
        -- initialize
        q.memd(m) <= ZEROMEM;
        
        if m = memory_map.zy(lcnt).memory_index and mem = '1'
        then
            q.memd(m).wsel <= memory_map.zy(lcnt).poly_index;
            q.memd(m).waddr <= addr_bitrev;
            q.memd(m).wdata <= current_sample_converted;
            q.memd(m).wen <= (others => memcnt_delayed(6));
        end if;
    end loop;
end process;




states: process(clk)
begin
    if rising_edge(clk)
    then
        if lcntq.ovf = '1'
        then
            state <= idle;
        else
            state <= nextstate;
        end if;
    end if;
end process;


signals: process(state, d, abscntq, memcntq, sqzcntq, memcnt_delayed)
begin
    nextstate <= state;
    
    q.ready <= '0';
    mem <= '1';
    sampleconv_en <= '1';
    
    q.fifoyzd.en <= '1';
    q.fifoyzd.rst <= '0';
    q.fifoyzd.ready_rcv <= '0';
    q.fifoyzd.valid <= '0';
    
    case state is
        when idle =>
            -- output signals
            q.ready <= '1';
            
            -- memory deactivated in idle state
            mem <= '0';
            
            -- keccak and memory
            q.keccakd.en <= '0';
            q.keccakd.rst <= '1';
            
            -- internal signals
            sampleconv_en <= '0';
            memcntd.rst <= '1';
            lcntd.en <= '0';
            lcntd.rst <= '1';
            abscntd.en <= '0';
            abscntd.rst <= '1';
            sqzcntd.en <= '0';
            sqzcntd.rst <= '1';
            
            memcntd.en <= '0';
            
            q.fifoyzd.rst <= '1';
            
            -- transition
            if d.en = '1' and d.keccakq.ready = '1'
            then
                nextstate <= absorb;
            end if;
            
        when absorb =>
            -- keccak and memory
            q.keccakd.en <= '1';
            q.keccakd.rst <= '0';
            
            -- set internal signals
            memcntd.rst <= '0';
            lcntd.en <= '0';
            lcntd.rst <= '0';
            abscntd.en <= '1';
            abscntd.rst <= '0';
            sqzcntd.en <= '0';
            sqzcntd.rst <= '1';
            
            memcntd.en <= '0';
            
            -- transition
            if abscntq.max = '1'
            then
                nextstate <= permute; -- then wait for result
            end if;
            
        when zeropad =>
            -- keccak and memory
            q.keccakd.en <= '1';
            q.keccakd.rst <= '0';
            
            -- set internal signals
            memcntd.rst <= '0';
            lcntd.en <= '0';
            lcntd.rst <= '0';
            abscntd.en <= '0';
            abscntd.rst <= '0';
            sqzcntd.en <= '0';
            sqzcntd.rst <= '0';
            
            memcntd.en <= '0';
            
            -- transition
            if d.keccakq.ready = '0'
            then
                nextstate <= permute; -- wait for permutation to finish
            end if;
            
        when permute =>
            -- keccak and memory
            q.keccakd.en <= '0';
            q.keccakd.rst <= '0';
            
            -- set internal signals
            memcntd.rst <= '0';
            lcntd.en <= '0';
            lcntd.rst <= '0';
            abscntd.en <= '0';
            abscntd.rst <= '0';
            sqzcntd.en <= '0';
            sqzcntd.rst <= '1';
            
            memcntd.en <= '0';
            
            -- transition
            if d.keccakq.ready = '1'
            then
                nextstate <= squeeze; -- wait for permutation to finish
            end if;
            
        when squeeze =>
            -- keccak and memory
            q.keccakd.en <= '1';
            q.keccakd.rst <= '0';
            
            -- set internal signals
            memcntd.rst <= '0';
            lcntd.en <= '0';
            lcntd.rst <= '0';
            abscntd.en <= '0';
            abscntd.rst <= '0';
            sqzcntd.en <= '1';
            sqzcntd.rst <= '0';
            
            q.fifoyzd.valid <= '1';
            memcntd.en <= '0';
            
            -- transition
            if memcnt_delayed(7) = '1' -- OVF
            then
                lcntd.en <= '1';
                nextstate <= reset_absorb;
            elsif sqzcntq.max = '1'
            then
                nextstate <= zeropad;
            elsif d.fifoyzq.toggle = '1'
            then
                nextstate <= sample; -- wait for reading SREG_WIDTH/32 from keccak
            end if;
            
        when sample =>
            -- keccak and memory
            q.keccakd.en <= '0';
            q.keccakd.rst <= '0';
            
            -- set internal signals
            memcntd.rst <= '0';
            lcntd.en <= '0';
            lcntd.rst <= '0';
            abscntd.en <= '0';
            abscntd.rst <= '0';
            sqzcntd.en <= '0';
            sqzcntd.rst <= '0';
            
            memcntd.en <= '1';
            q.fifoyzd.ready_rcv <= '1';
            
            -- transition
            if memcnt_delayed(7) = '1' -- OVF
            then
                lcntd.en <= '1';
                nextstate <= reset_absorb;
            elsif d.fifoyzq.toggle = '1' -- wait for sampling SREG_WIDTH/loggamma1 words to finish
            then
                if sqzcntq.ovf = '1'
                then
                    nextstate <= zeropad;
                else
                    nextstate <= squeeze;
                end if;
            end if;
            
        when reset_absorb =>
            -- keccak and memory
            q.keccakd.en <= '0';
            q.keccakd.rst <= '1';
            
            -- set internal signals
            memcntd.rst <= '1';
            lcntd.en <= '0';
            lcntd.rst <= '0';
            abscntd.en <= '0';
            abscntd.rst <= '1';
            sqzcntd.en <= '0';
            sqzcntd.rst <= '1';
            
            memcntd.en <= '0';
            q.fifoyzd.rst <= '1';
            
            -- transition
            if d.keccakq.ready = '1'
            then
                nextstate <= absorb;
            end if;
    end case;
end process;

--watch: process(clk)
--begin
--    if rising_edge(clk)
--    then
--        if memcnt_delayed(6) = '1'
--        then
--            for i in 0 to 3
--            loop
--                report integer'image(to_integer(unsigned(current_sample_converted(i))));
--            end loop;
--        end if;
--    end if;
--end process;

end Behavioral;
