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

entity expandA is
    Port (
        clk : in std_logic;
        d   : in expand_in_type;
        q   : out expand_out_type
    );
end expandA;

architecture Behavioral of expandA is

    type state_type is (idle, absorb, permute, squeeze, sample, reset_absorb);
    signal state, nextstate : state_type := idle;
    
    -- sample register
    signal sampledata : std_logic_vector(95 downto 0);
    signal current_sample, sample_tmp : std_logic_vector(22 downto 0);
    signal sample_accept, counter_en, sampledata_en_squeeze, sampledata_en_sample : std_logic;
    signal wen : std_logic_vector(0 to 3);
    
    -- address tmp
    signal addr, addr_bitrev : coef_addr_array(0 to 3);
    
    -- k and l counter
    signal k : natural range 0 to DILITHIUM_k-1 := 0;
    signal l : natural range 0 to DILITHIUM_l := 0;
    signal kl_en, kl_ovf, kl_rst : std_logic;
    
    -- absorb counter
    signal abscntd : counter_in_type;
    signal abscntq : counter_out_type;
    signal abscnt : natural range 0 to SHAKE128_RATE/32;
    
    -- memory count
    signal memcnt_en, memcnt_rst, memcnt_ovf : std_logic;
    signal memcnt : std_logic_vector(7 downto 0);
    
    -- sample counter
    signal samplecntd, readcntd : counter_in_type;
    signal samplecntq, readcntq : counter_out_type;
    signal readcnt_rst_override, samplecnt_rst_override : std_logic;
    
    -- squeeze counter
    constant SQZCNT_MAX : natural := (SHAKE128_RATE/32)-1;
    signal sqzcntd : counter_in_type;
    signal sqzcntq : counter_out_type;
    signal sqzcnt : natural range 0 to SQZCNT_MAX := 0;
    
    -- keccak
    signal keccak_din : std_logic_vector(31 downto 0);
    
    -- memory
    signal mem : std_logic;

begin


-- k and l counter
klcounter: process(clk)
begin
    if rising_edge(clk)
    then
        if kl_rst = '1'
        then
            l <= 0;
            k <= 0;
            kl_ovf <= '0';
        elsif kl_en = '1' and kl_ovf = '0'
        then
            k <= k + 1;
            if k = DILITHIUM_k - 1
            then
                k <= 0;
                l <= l + 1;
                if l = DILITHIUM_l - 1
                then
                    l <= 0;
                    kl_ovf <= '1';
                end if;
            end if;
        end if; -- en = '1' and ovf = '0'
    end if; -- rising_edge
end process;


-- sample data register
sampledatareg: process(clk)
begin
    if rising_edge(clk)
    then
        if sampledata_en_squeeze = '1'
        then
            sampledata(95 downto 32) <= sampledata(63 downto 0);
            sampledata(31 downto 0) <= d.keccakq.data;
        elsif sampledata_en_sample = '1'
        then
            sampledata(95 downto 24) <= sampledata(71 downto 0);
            sampledata(23 downto 0) <= (others => '0');
        end if;
    end if;
end process;
current_sample(22 downto 16) <= sampledata(78 downto 72); -- drop msb
current_sample(15 downto  8) <= sampledata(87 downto 80);
current_sample( 7 downto  0) <= sampledata(95 downto 88); 


-- memory counter: offset for the coefficients that go into memory
memory_counter: process(clk)
begin
    if rising_edge(clk)
    then
        if memcnt_rst = '1'
        then
            wen <= "1000";
            memcnt <= (others => '0');
            memcnt_ovf <= '0';
        elsif memcnt_en = '1' and memcnt_ovf = '0'
        then
            wen <= wen(3) & wen(0 to 2); -- rotate
            memcnt <= std_logic_vector(unsigned(memcnt) + 1);
            if unsigned(memcnt) = DILITHIUM_N-1
            then
                memcnt_ovf <= '1';
            end if;
        end if;
    end if;
end process;






absorb_counter: entity work.counter
generic map (max_value => SHAKE128_RATE/32)
port map (
    clk => clk,
    d => abscntd,
    q => abscntq,
    value => abscnt
);


absorbmux: process(d.seedregq, k, l, abscnt)
begin
    case abscnt is
        when 0 =>
            q.seedregd.en_rotate <= '0';
            q.seedregd.en_write <= '0';
            keccak_din <= (others => '0');
        when 1 to 8 =>
            q.seedregd.en_rotate <= '1';
            q.seedregd.en_write <= '0';
            keccak_din <= d.seedregq.data;
        when 9 =>
            q.seedregd.en_rotate <= '0';
            q.seedregd.en_write <= '0';
            keccak_din(31 downto 24) <= std_logic_vector(to_unsigned(l, 8));
            keccak_din(23 downto 16) <= std_logic_vector(to_unsigned(k, 8));
            keccak_din(15 downto  0) <= x"1f00";
        when 42 => -- shake 128
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

-- sample counter
sample_counter: entity work.counter -- samples from sample register
generic map (max_value => 96/24-1)
port map (
    clk => clk,
    d => samplecntd,
    q => samplecntq,
    value => open
);

read_counter: entity work.counter -- read from keccak to sample register
generic map (max_value => 2)
port map (
    clk => clk,
    d => readcntd,
    q => readcntq,
    value => open
);
memcnt_en <= counter_en and sample_accept;

-- keccak
q.keccakd.data <= keccak_din when abscntd.en = '1' else d.keccakq.data;





-- sampling
sample_tmp <= current_sample;
sample_accept <= '1' when unsigned(sample_tmp) < DILITHIUM_Q else '0';


addr(0)(7 downto 2) <= memcnt(7 downto 2);
addr(1)(7 downto 2) <= memcnt(7 downto 2);
addr(2)(7 downto 2) <= memcnt(7 downto 2);
addr(3)(7 downto 2) <= memcnt(7 downto 2);
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

memmux: process(addr_bitrev, k, l, mem, sample_tmp, wen, memcnt_ovf, memcnt_en)
begin
    for m in 0 to NUM_MEM_8_POLY-1
    loop
        -- initialize
        q.memd(m) <= ZEROMEM;
        
        if m = memory_map.A(k, l).memory_index and mem = '1'
        then
            q.memd(m).wsel <= memory_map.A(k, l).poly_index;
            q.memd(m).waddr <= addr_bitrev;
            q.memd(m).wdata <= (others => sample_tmp);
            if memcnt_en = '1' and memcnt_ovf = '0' then
                q.memd(m).wen <= wen;
            end if;
        end if;
    end loop;
end process;




states: process(clk)
begin
    if rising_edge(clk)
    then
        if kl_ovf = '1'
        then
            state <= idle;
        else
            state <= nextstate;
        end if;
    end if;
end process;


signals: process(state, d.en, d.keccakq.ready, abscntq.max, memcnt_ovf, readcntq.max, samplecntq.max, sqzcntq.ovf)
begin
    nextstate <= state;
    
    q.ready <= '0';
    mem <= '1';
    
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
            kl_en <= '0';
            kl_rst <= '1';
            counter_en <= '0';
            memcnt_rst <= '1';
            abscntd.en <= '0';
            abscntd.rst <= '1';
            
            sqzcntd.en <= '0';
            sqzcntd.rst <= '1';
            readcntd.en <= '0';
            readcntd.rst <= '1';
            samplecntd.en <= '0';
            samplecntd.rst <= '1';
            
            sampledata_en_squeeze <= '0';
            sampledata_en_sample <= '0';
            
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
            kl_en <= '0';
            kl_rst <= '0';
            counter_en <= '0';
            memcnt_rst <= '0';
            abscntd.en <= '1';
            abscntd.rst <= '0';
            
            sqzcntd.en <= '0';
            sqzcntd.rst <= '1';
            readcntd.en <= '0';
            readcntd.rst <= '1';
            samplecntd.en <= '0';
            samplecntd.rst <= '1';
            
            sampledata_en_squeeze <= '0';
            sampledata_en_sample <= '0';
            
            -- transition
            if abscntq.max = '1'
            then
                nextstate <= permute; -- then wait for result
            end if;
            
        when permute =>
            -- keccak and memory
            q.keccakd.en <= '0';
            q.keccakd.rst <= '0';
            
            -- set internal signals
            kl_en <= '0';
            kl_rst <= '0';
            counter_en <= '0';
            memcnt_rst <= '0';
            abscntd.en <= '0';
            abscntd.rst <= '0';
            
            sqzcntd.en <= '0';
            sqzcntd.rst <= '1';
            readcntd.en <= '0';
            readcntd.rst <= '0';
            samplecntd.en <= '0';
            samplecntd.rst <= '0';
            
            sampledata_en_squeeze <= '0';
            sampledata_en_sample <= '0';
            
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
            kl_en <= '0';
            kl_rst <= '0';
            counter_en <= '0';
            memcnt_rst <= '0';
            abscntd.en <= '0';
            abscntd.rst <= '0';
            
            sqzcntd.en <= '1';
            sqzcntd.rst <= '0';
            readcntd.en <= '1';
            readcntd.rst <= '0';
            samplecntd.en <= '0';
            samplecntd.rst <= '1';
            
            sampledata_en_squeeze <= '1';
            sampledata_en_sample <= '0';
            
            -- transition
            if memcnt_ovf = '1'
            then
                kl_en <= '1';
                nextstate <= reset_absorb;
            elsif readcntq.max = '1'
            then
                nextstate <= sample; -- wait for reading 96/32 words to finish
            end if;
            
        when sample =>
            -- keccak and memory
            q.keccakd.en <= '0';
            q.keccakd.rst <= '0';
            
            -- set internal signals
            kl_en <= '0';
            kl_rst <= '0';
            counter_en <= '1';
            memcnt_rst <= '0';
            abscntd.en <= '0';
            abscntd.rst <= '0';
            
            sqzcntd.en <= '0';
            sqzcntd.rst <= '0';
            readcntd.en <= '0';
            readcntd.rst <= '1';
            samplecntd.en <= '1';
            samplecntd.rst <= '0';
            
            sampledata_en_squeeze <= '0';
            sampledata_en_sample <= '1';
            
            -- transition
            if memcnt_ovf = '1'
            then
                kl_en <= '1';
                nextstate <= reset_absorb;
            elsif samplecntq.max = '1' -- wait for sampling 96/24 words to finish
            then
                if sqzcntq.ovf = '1'
                then
                    nextstate <= permute;
                else
                    nextstate <= squeeze;
                end if;
            end if;
            
        when reset_absorb =>
            -- keccak and memory
            q.keccakd.en <= '0';
            q.keccakd.rst <= '1';
            
            -- set internal signals
            kl_en <= '0';
            kl_rst <= '0';
            counter_en <= '0';
            memcnt_rst <= '1';
            abscntd.en <= '0';
            abscntd.rst <= '1';
            
            sqzcntd.en <= '0';
            sqzcntd.rst <= '1';
            readcntd.en <= '0';
            readcntd.rst <= '1';
            samplecntd.en <= '0';
            samplecntd.rst <= '1';
            
            sampledata_en_squeeze <= '0';
            sampledata_en_sample <= '0';
            
            -- transition
            if d.keccakq.ready = '1'
            then
                nextstate <= absorb;
            end if;
    end case;
end process;

end Behavioral;
