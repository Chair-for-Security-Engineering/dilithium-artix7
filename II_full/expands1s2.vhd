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

entity expands1s2 is
    Port (
        clk : in std_logic;
        d   : in expand_in_type;
        q   : out expand_out_type
    );
end expands1s2;

architecture Behavioral of expands1s2 is

    type state_type is (idle, absorb, permute, squeeze, sample, flush, reset_absorb);
    signal state, nextstate : state_type := idle;
    
    signal current_sample : std_logic_vector(3 downto 0);
    signal sample_tmp : std_logic_vector(22 downto 0);
    signal sample_accept, counter_en, sampledata_en_squeeze, sampledata_en_sample : std_logic;
    signal wen : std_logic_vector(0 to 3);
    
    -- address tmp
    signal addr, addr_bitrev : coef_addr_array(0 to 3);
    
    -- k and l counter
    signal k : natural range 0 to DILITHIUM_k := 0;
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
    signal samplecntd : counter_in_type;
    signal samplecntq : counter_out_type;
    signal samplecnt : natural range 0 to 32/4-1;
    
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


-- k and l counter
klcounter: process(clk)
begin
    if rising_edge(clk)
    then
        if kl_rst = '1'
        then
            l <= 0;
            k <= INACTIVE_k;
            kl_ovf <= '0';
        elsif kl_en = '1' and kl_ovf = '0'
        then
            if l < DILITHIUM_l
            then
                l <= l + 1;
                k <= INACTIVE_k;
                if l = DILITHIUM_l-1
                then
                    k <= 0;
                end if;
            else
                k <= k + 1;
                if k = DILITHIUM_k-1
                then
                    kl_ovf <= '1';
                end if;
            end if;
        end if; -- en = '1' and ovf = '0'
    end if; -- rising_edge
end process;



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
        when 1 to 16 =>
            q.seedregd.en_rotate <= '1';
            q.seedregd.en_write <= '0';
            keccak_din <= d.seedregq.data;
        when 17 =>
            q.seedregd.en_rotate <= '0';
            q.seedregd.en_write <= '0';
            if k = INACTIVE_k
            then
                keccak_din(31 downto 24) <= std_logic_vector(to_unsigned(l, 8));
            else
                keccak_din(31 downto 24) <= std_logic_vector(to_unsigned(k+l, 8));
            end if;
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


samplemux: process(samplecnt, d.keccakq.data)
variable b : natural;
begin
    b := 3-samplecnt/2;
    case samplecnt is
        when 0|2|4|6 =>
            current_sample <= d.keccakq.data(b*8+3 downto b*8);
        when 1|3|5|7 =>
            current_sample <= d.keccakq.data(b*8+7 downto b*8+4);
        when others =>
            current_sample <= (others => '0');
            assert false report "bad samplecnt" severity failure;
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
sample_counter: entity work.counter -- samples from keccak out register
generic map (max_value => 32/4-1)
port map (
    clk => clk,
    d => samplecntd,
    q => samplecntq,
    value => samplecnt
);


memcnt_en <= counter_en and sample_accept;

-- keccak
q.keccakd.data <= keccak_din when abscntd.en = '1' else d.keccakq.data;





-- sampling
eta2: if DILITHIUM_eta = 2
generate
    sample_tmp <= std_logic_vector(to_unsigned((2-(to_integer(unsigned(current_sample)) mod 5)) mod DILITHIUM_Q, 23));
    sample_accept <= '1' when unsigned(current_sample) < 15 else '0';
end generate;
eta4: if DILITHIUM_eta = 4
generate
    sample_tmp <= std_logic_vector(to_unsigned((4-to_integer(unsigned(current_sample))) mod DILITHIUM_Q, 23));
    sample_accept <= '1' when unsigned(current_sample) < 9 else '0';
end generate;



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
type mpty is array(0 to 2) of natural;
variable m,p : mpty;
begin
    -- initialize
    q.memd <= (others => ZEROMEM);
    
    if k = INACTIVE_k
    then
        if l < DILITHIUM_l
        then
            m(0) := memory_map.s1(l).memory_index;
            p(0) := memory_map.s1(l).poly_index;
            m(1) := memory_map.t1(l).memory_index;
            p(1) := memory_map.t1(l).poly_index;
            for i in 0 to 1
            loop
                q.memd(m(i)).wsel <= p(i);
                q.memd(m(i)).waddr <= addr_bitrev;
                q.memd(m(i)).wdata <= (others => sample_tmp);
                if memcnt_en = '0' or memcnt_ovf = '1' then
                    q.memd(m(i)).wen <= "0000";
                else
                    q.memd(m(i)).wen <= wen;
                end if;
            end loop;
        end if;
    else
        m(2) := memory_map.s2(k).memory_index;
        p(2) := memory_map.s2(k).poly_index;
        q.memd(m(2)).wsel <= p(2);
        q.memd(m(2)).waddr <= addr_bitrev;
        q.memd(m(2)).wdata <= (others => sample_tmp);
        if memcnt_en = '0' or memcnt_ovf = '1' then
            q.memd(m(2)).wen <= "0000";
        else
            q.memd(m(2)).wen <= wen;
        end if;
    end if;
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


signals: process(state, d.en, d.keccakq.ready, abscntq.max, memcnt_ovf, samplecntq.max, sqzcntq.max)
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
            samplecntd.en <= '0';
            samplecntd.rst <= '1';
            
            sampledata_en_squeeze <= '0';
            sampledata_en_sample <= '0';
            
            -- transition
            if d.keccakq.ready = '1'
            then
                nextstate <= sample; -- wait for permutation to finish
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
            samplecntd.en <= '0';
            samplecntd.rst <= '1';
            
            sampledata_en_squeeze <= '1';
            sampledata_en_sample <= '0';
            
            -- transition
            nextstate <= sample; -- rotate only one further
            if memcnt_ovf = '1'
            then
                kl_en <= '1';
                nextstate <= reset_absorb;
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
                if sqzcntq.max = '1'
                then
                    nextstate <= flush; -- squeeze/rotate until whole shake128 state is rotated (we only need shake256)
                else
                    nextstate <= squeeze;
                end if;
            end if;
            
        when flush =>
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
            
            sqzcntd.en <= '0';
            sqzcntd.rst <= '0';
            samplecntd.en <= '0';
            samplecntd.rst <= '0';
            
            sampledata_en_squeeze <= '0';
            sampledata_en_sample <= '0';
            
            -- transition
            if d.keccakq.ready = '0'
            then
                nextstate <= permute;
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
