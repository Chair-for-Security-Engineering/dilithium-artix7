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

entity load is
    Port (
        clk : in std_logic;
        d   : in load_in_type;
        q   : out load_out_type
    );
end load;

architecture Behavioral of load is

type state_type is (idle, l_rho, l_K, l_tr, l_t1, l_t1_send, l_s1, l_s1_send, l_s2, l_s2_send, l_t0, l_t0_send);
signal state, nextstate : state_type;

signal regcntd, memcntd : counter_in_type;
signal regcntq, memcntq : counter_out_type;
signal regcnt : natural range 0 to DILITHIUM_omega+DILITHIUM_k-1;
signal memcnt : natural range 0 to DILITHIUM_k*DILITHIUM_N/4-1;

signal t1rmcntd, t1rlcntd : counter_in_type;
signal t1rmcntq, t1rlcntq : counter_out_type;
signal t1rmcnt : natural range 0 to 160/40+GLOBAL_MEMORY_DELAY-1;
signal t1reg : std_logic_vector(159 downto 0);
signal t1r_en_mem, t1r_en_load : std_logic;
signal t1reg_in : std_logic_vector(39 downto 0);
signal t1reg_out : std_logic_vector(31 downto 0);

signal t0rmcntd, t0rlcntd : counter_in_type;
signal t0rmcntq, t0rlcntq : counter_out_type;
signal t0rmcnt : natural range 0 to 416/52+GLOBAL_MEMORY_DELAY+DELAY_CONV_YZ-1;
signal t0reg : std_logic_vector(415 downto 0);
signal t0r_en_mem, t0r_en_load : std_logic;
signal t0reg_in : std_logic_vector(51 downto 0);
signal t0reg_out : std_logic_vector(31 downto 0);

constant ZREG_WIDTH : natural := 288 - (DILITHIUM_loggamma1/19)*128;
signal zrmcntd, zrlcntd : counter_in_type;
signal zrmcntq, zrlcntq : counter_out_type;
signal zrmcnt : natural range 0 to ZREG_WIDTH/((DILITHIUM_loggamma1+1)*4)+GLOBAL_MEMORY_DELAY+DELAY_CONV_YZ-1;
signal zreg : std_logic_vector(ZREG_WIDTH - 1 downto 0);
signal zr_en_mem, zr_en_load : std_logic;
signal zreg_in : std_logic_vector((DILITHIUM_loggamma1+1)*4-1 downto 0);
signal zreg_out : std_logic_vector(31 downto 0);
    
signal etareg : std_logic_vector(95 downto 0);
signal etareg_en_load, etareg_en_store : std_logic;
signal etareg_in : std_logic_vector(15 downto 0);
signal etareg_in_unconvd : payload_array(0 to 3);
signal etareg_out : std_logic_vector(31 downto 0);
signal elcntd, escntd : counter_in_type;
signal elcntq, escntq : counter_out_type;
constant ESCNT_MAX : natural := GLOBAL_MEMORY_DELAY+96/12-(96/12-32/16)*(DILITHIUM_eta/4)-1;
signal escnt : natural range 0 to ESCNT_MAX;

type pipeline_type is array(1 to GLOBAL_MEMORY_DELAY) of natural range 0 to NUM_MEM_8_POLY-1;
signal pipeline : pipeline_type := (others => 0);
signal pip_in, pip_out : natural range 0 to NUM_MEM_8_POLY-1 := 0;

signal memq : payload_array(0 to 3);

begin
--------------------------------------------------------------------------------------------------
-- eta polynomials buffer register
--------------------------------------------------------------------------------------------------
eta2reg: if DILITHIUM_eta = 2 -- use the whole 96 bit register
generate
    eta_register: process(clk)
    begin
        if rising_edge(clk)
        then
            if etareg_en_load = '1'
            then
                etareg(63 downto 0) <= etareg(95 downto 32);
                etareg(95 downto 64) <= (others => '0');
            elsif etareg_en_store = '1'
            then
                etareg(95 downto 96-12) <= etareg_in(11 downto 0);
                etareg(95-12 downto 0) <= etareg(95 downto 12);
            end if;
        end if;
    end process;
    eriogen: for i in 0 to 3
    generate
        etareg_out(31-8*i downto 32-8*i-8) <= etareg(8*i+7 downto 8*i);
        with memq(i)
        select
            etareg_in(i*3+2 downto i*3) <= 
                "100" when "11111111101111111111111",
                "011" when "11111111110000000000000",
                "010" when "00000000000000000000000",
                "001" when "00000000000000000000001",
                "000" when "00000000000000000000010",
                "000" when others; -- invalid
    end generate;
    eta_load_counter: entity work.counter
    generic map (max_value => 96/32-1)
    port map (
        clk => clk,
        d => elcntd,
        q => elcntq,
        value => open
    );
    eta_store_counter: entity work.counter
    generic map (max_value => ESCNT_MAX)
    port map (
        clk => clk,
        d => escntd,
        q => escntq,
        value => escnt
    );
end generate;
eta4reg: if DILITHIUM_eta = 4 -- use the lower 32 bits only
generate
    eta_register: process(clk)
    begin
        if rising_edge(clk)
        then
            if etareg_en_load = '1'
            then
                etareg(15 downto 0) <= etareg(31 downto 16);
                etareg(31 downto 16) <= (others => '0');
            elsif etareg_en_store = '1'
            then
                etareg(31 downto 16) <= etareg_in;
                etareg(15 downto 0) <= etareg(31 downto 16);
            end if;
        end if;
    end process;
    eriogen: for i in 0 to 3
    generate
        etareg_out(31-8*i downto 32-8*i-8) <= etareg(8*i+7 downto 8*i);
        with memq(i)
        select
            etareg_in(i*4+3 downto i*4) <= 
                "1000" when "11111111101111111111101",
                "0111" when "11111111101111111111110",
                "0110" when "11111111101111111111111",
                "0101" when "11111111110000000000000",
                "0100" when "00000000000000000000000",
                "0011" when "00000000000000000000001",
                "0010" when "00000000000000000000010",
                "0001" when "00000000000000000000011",
                "0000" when "00000000000000000000100",
                "0000" when others; -- invalid
    end generate;
    eta_store_counter: entity work.counter
    generic map (max_value => ESCNT_MAX)
    port map (
        clk => clk,
        d => escntd,
        q => escntq,
        value => escnt
    );
    elcntq.max <= '1';
end generate;

----------------------------------------------------------------------------------------------------
-- t1 register
----------------------------------------------------------------------------------------------------
t1r: process(clk)
begin
    if rising_edge(clk)
    then
        if t1r_en_mem = '1'
        then
            for i in 0 to 4
            loop
                t1reg(8*i+7 downto 8*i) <= t1reg_in(39-8*i downto 40-8*i-8);
            end loop;
            t1reg(159 downto 40) <= t1reg(119 downto 0);
        elsif t1r_en_load = '1'
        then
            t1reg(159 downto 32) <= t1reg(127 downto 0);
            t1reg(31 downto 0) <= (others => '0');
        end if;
    end if;
end process;
t1reg_out <= t1reg(159 downto 128);
t1rigen: for i in 0 to 3
generate
    t1reg_in(i*10+9 downto i*10) <= memq(i)(22 downto 13);
end generate;

----------------------------------------------------------------------------------------------------
-- t0 register
----------------------------------------------------------------------------------------------------
t0r: process(clk)
begin
    if rising_edge(clk)
    then
        if t0r_en_mem = '1'
        then
            t0reg(415 downto 416-52) <= t0reg_in;
            t0reg(415-52 downto 0) <= t0reg(415 downto 52);
        elsif t0r_en_load = '1'
        then
            t0reg(415-32 downto 0) <= t0reg(415 downto 32);
            t0reg(415 downto 416-32) <= (others => '0');
        end if;
    end if;
end process;
t0rogen: for i in 0 to 3
generate 
    t0reg_out(i*8+7 downto i*8) <= t0reg(31-i*8 downto 32-i*8-8);
    t0reg_in(i*13+12 downto i*13) <= d.convyzq(i)(12 downto 0);
end generate;

----------------------------------------------------------------------------------------------------
-- convyzd input
----------------------------------------------------------------------------------------------------
q.convyzd.data <= memq;

----------------------------------------------------------------------------------------------------
-- payload mux
----------------------------------------------------------------------------------------------------
with state 
select
    q.payload <=    t0reg_out when l_t0 | l_t0_send,
                    t1reg_out when l_t1 | l_t1_send,
                    etareg_out when l_s1 | l_s2 | l_s1_send | l_s2_send,
                    d.rhoregq.data when l_rho,
                    d.Kregq.data when l_K,
                    d.trregq.data when l_tr,
                    (others => '0') when others;
                    
----------------------------------------------------------------------------------------------------
-- register input
----------------------------------------------------------------------------------------------------
q.rhoregd.data <= (others => '0');
q.Kregd.data <= (others => '0');
q.trregd.data <= (others => '0');
q.chashregd.data <= (others => '0');
q.rhoregd.en_write <= '0';
q.Kregd.en_write <= '0';
q.trregd.en_write <= '0';
q.chashregd.en_write <= '0';
rotmux: process(state, regcntd.en)
begin
    q.rhoregd.en_rotate <= '0';
    q.Kregd.en_rotate <= '0';
    q.trregd.en_rotate <= '0';
    q.chashregd.en_rotate <= '0';
    case state is
        when l_rho => q.rhoregd.en_rotate <= regcntd.en;
        when l_K => q.Kregd.en_rotate <= regcntd.en;
        when l_tr => q.trregd.en_rotate <= regcntd.en;
        when others =>
    end case;
end process;

----------------------------------------------------------------------------------------------------
-- counter
----------------------------------------------------------------------------------------------------
reg_counter: entity work.counter
generic map (max_value => DILITHIUM_omega+DILITHIUM_k-1)
port map (
    clk => clk,
    d => regcntd,
    q => regcntq,
    value => regcnt
);

memory_counter: entity work.counter
generic map (max_value => DILITHIUM_k*DILITHIUM_N/4-1)
port map (
    clk => clk,
    d => memcntd,
    q => memcntq,
    value => memcnt
);

t1r_load_counter: entity work.counter
generic map (max_value => 160/32-1)
port map (
    clk => clk,
    d => t1rlcntd,
    q => t1rlcntq,
    value => open
);

t1r_mem_counter: entity work.counter
generic map (max_value => 160/40+GLOBAL_MEMORY_DELAY-1)
port map (
    clk => clk,
    d => t1rmcntd,
    q => t1rmcntq,
    value => t1rmcnt
);

t0r_load_counter: entity work.counter
generic map (max_value => 416/32-1)
port map (
    clk => clk,
    d => t0rlcntd,
    q => t0rlcntq,
    value => open
);

t0r_mem_counter: entity work.counter
generic map (max_value => 416/52+GLOBAL_MEMORY_DELAY+DELAY_CONV_YZ-1)
port map (
    clk => clk,
    d => t0rmcntd,
    q => t0rmcntq,
    value => t0rmcnt
);

z_load_counter: entity work.counter
generic map (max_value => ZREG_WIDTH/32-1)
port map (
    clk => clk,
    d => zrlcntd,
    q => zrlcntq,
    value => open
);

z_mem_counter: entity work.counter
generic map (max_value => ZREG_WIDTH/((DILITHIUM_loggamma1+1)*4)+GLOBAL_MEMORY_DELAY+DELAY_CONV_YZ-1)
port map (
    clk => clk,
    d => zrmcntd,
    q => zrmcntq,
    value => zrmcnt
);

----------------------------------------------------------------------------------------------------
-- memory mux
----------------------------------------------------------------------------------------------------
pip: process(clk)
begin
    if rising_edge(clk)
    then
        for i in GLOBAL_MEMORY_DELAY downto 2
        loop
            pipeline(i) <= pipeline(i-1);
        end loop;
        pipeline(1) <= pip_in;
    end if;
end process;
pip_out <= pipeline(GLOBAL_MEMORY_DELAY);
memmux: process(state, d, memcnt, memcntd, pip_out)
variable m, p : natural;
variable addr, revaddr : coef_addr_array(0 to 3);
begin
    m := 0;

    for i in 0 to 3
    loop
        addr(i) := std_logic_vector(to_unsigned(memcnt mod (DILITHIUM_N/4), 6)) & std_logic_vector(to_unsigned(i, 2));
        for j in 0 to 7
        loop
            revaddr(i)(j) := addr(i)(7-j);
        end loop;
    end loop;

    q.memd <= (others => ZEROMEM);

    case state is 
        when l_t1 | l_t1_send =>
            m := memory_map.t1(memcnt / (DILITHIUM_N/4)).memory_index;
            p := memory_map.t1(memcnt / (DILITHIUM_N/4)).poly_index;
            q.memd(m).rsel <= p;
            q.memd(m).raddr <= revaddr;
            q.memd(m).ren <= (others => memcntd.en);
            
        when l_t0 | l_t0_send =>
            m := memory_map.t0(memcnt / (DILITHIUM_N/4)).memory_index;
            p := memory_map.t0(memcnt / (DILITHIUM_N/4)).poly_index;
            q.memd(m).rsel <= p;
            q.memd(m).raddr <= revaddr;
            q.memd(m).ren <= (others => memcntd.en);
            
        when l_s1 | l_s1_send =>
            m := memory_map.s1((memcnt / (DILITHIUM_N/4)) mod DILITHIUM_l).memory_index;
            p := memory_map.s1((memcnt / (DILITHIUM_N/4)) mod DILITHIUM_l).poly_index;
            assert (memcnt / (DILITHIUM_N/4) < DILITHIUM_l) report "memcnt / (DILITHIUM_N/4) >= DILITHIUM_l" severity warning;
            q.memd(m).rsel <= p;
            q.memd(m).raddr <= revaddr;
            q.memd(m).ren <= (others => memcntd.en);
            
        when l_s2 | l_s2_send =>
            m := memory_map.s2(memcnt / (DILITHIUM_N/4)).memory_index;
            p := memory_map.s2(memcnt / (DILITHIUM_N/4)).poly_index;
            q.memd(m).rsel <= p;
            q.memd(m).raddr <= revaddr;
            q.memd(m).ren <= (others => memcntd.en);
    
        when others =>
    end case;
    
    pip_in <= m;
    memq <= d.memq(pip_out);

end process;

----------------------------------------------------------------------------------------------------
-- state machine
----------------------------------------------------------------------------------------------------
states: process(clk)
begin
    if rising_edge(clk)
    then
        state <= nextstate;
    end if;
end process;

signals: process(state, d, regcntq, regcnt, t1rmcnt, t1rmcntq, t1rlcntq, t0rmcnt, t0rmcntq, t0rlcntq, zrmcnt, zrmcntq, zrlcntq, escnt, elcntq, escntq, memcntq, memcnt)
begin
    nextstate <= state;
    
    q.ready <= '0';
    q.valid <= '0';

    regcntd.rst <= '0';
    regcntd.en <= '0';
    
    memcntd.en <= '0';
    memcntd.rst <= '0';
    
    t1rmcntd.en <= '0';
    t1rmcntd.rst <= '0'; 
    t1rlcntd.en <= '0'; 
    t1rlcntd.rst <= '0';
    t1r_en_mem <= '0';
    t1r_en_load <= '0';
    
    t0rmcntd.en <= '0';
    t0rmcntd.rst <= '0'; 
    t0rlcntd.en <= '0'; 
    t0rlcntd.rst <= '0';
    t0r_en_mem <= '0';
    t0r_en_load <= '0';
    
    q.hregd.en_rotate_data <= '0';
    q.hregd.en_rotate_poly <= '0';

    q.convyzd.en <= '0';
    q.convyzd.sub <= std_logic_vector(to_unsigned(2**DILITHIUM_loggamma1, 23));
    
    etareg_en_store <= '0';
    etareg_en_load <= '0';
    escntd.en <= '0';
    escntd.rst <= '0';
    elcntd.en <= '0';
    elcntd.rst <= '0';
    
    case state is
        when idle =>
            q.ready <= '1';
            memcntd.rst <= '1';
            regcntd.rst <= '1';
            t1rmcntd.rst <= '1'; 
            t1rlcntd.rst <= '1';
            t0rmcntd.rst <= '1'; 
            t0rlcntd.rst <= '1';
            escntd.rst <= '1';
            elcntd.rst <= '1';
            if d.en = '1'
            then
                case d.payload_type is
                    when PAYLOAD_TYPE_PK => nextstate <= l_rho;
                    when PAYLOAD_TYPE_SK => nextstate <= l_rho; 
                    when others =>
                end case;
            end if;
        
        ----------------------------------------------------------------------------------------------------
        -- registers
        ----------------------------------------------------------------------------------------------------
        when l_rho | l_K | l_tr =>
            regcntd.en <= d.ready_rcv;
            q.valid <= '1';
            if regcnt = 256/32-1 and d.ready_rcv = '1'
            then
                regcntd.rst <= '1';
                case state is
                    when l_rho => 
                        case d.payload_type is
                            when PAYLOAD_TYPE_PK => nextstate <= l_t1;
                            when PAYLOAD_TYPE_SK => nextstate <= l_K;
                            when others => nextstate <= idle;
                        end case;
                    when l_K => nextstate <= l_tr;
                    when l_tr => nextstate <= l_s1;
                    when others => nextstate <= idle;
                end case;
            end if;
        
        ----------------------------------------------------------------------------------------------------
        -- t1
        ----------------------------------------------------------------------------------------------------
        when l_t1 =>
            if t1rmcnt <= 160/40-1
            then
                memcntd.en <= '1';
            end if;
            if t1rmcnt >= GLOBAL_MEMORY_DELAY-1
            then
                t1r_en_mem <= '1';
            end if;
            t1rmcntd.en <= '1';
            t1rlcntd.rst <= '1';
            if t1rmcntq.max = '1'
            then
                nextstate <= l_t1_send;
            end if;
        
        when l_t1_send =>
            q.valid <= '1';
            t1r_en_load <= d.ready_rcv;
            t1rlcntd.en <= d.ready_rcv;
            t1rmcntd.rst <= '1';
            if t1rlcntq.max = '1' and d.ready_rcv = '1'
            then
                if memcntq.ovf = '0'
                then
                    nextstate <= l_t1;
                else
                    nextstate <= idle; -- done loading pk
                end if;
            end if;
        
        ----------------------------------------------------------------------------------------------------
        -- t0
        ----------------------------------------------------------------------------------------------------
        when l_t0 =>
            if t0rmcnt <= 416/52-1
            then
                memcntd.en <= '1';
            end if;
            if t0rmcnt >= GLOBAL_MEMORY_DELAY+DELAY_CONV_YZ-1
            then
                t0r_en_mem <= '1';
            end if;
            q.convyzd.en <= '1';
            q.convyzd.sub <= std_logic_vector(to_unsigned(2**12, 23));
            t0rmcntd.en <= '1';
            t0rlcntd.rst <= '1';
            if t0rmcntq.max = '1'
            then
                nextstate <= l_t0_send;
            end if;
        
        when l_t0_send =>
            q.valid <= '1';
            t0r_en_load <= d.ready_rcv;
            t0rlcntd.en <= d.ready_rcv;
            t0rmcntd.rst <= '1';
            if t0rlcntq.max = '1' and d.ready_rcv = '1'
            then
                if memcntq.ovf = '0'
                then
                    nextstate <= l_t0;
                else
                    nextstate <= idle;
                end if;
            end if;
        
        ----------------------------------------------------------------------------------------------------
        -- s1
        ----------------------------------------------------------------------------------------------------
        when l_s1 =>
            if escnt <= ESCNT_MAX-GLOBAL_MEMORY_DELAY
            then
                memcntd.en <= '1';
            end if;
            if escnt >= GLOBAL_MEMORY_DELAY-1
            then
                etareg_en_store <= '1';
            end if;
            escntd.en <= '1';
            elcntd.rst <= '1';
            if escntq.max = '1'
            then
                nextstate <= l_s1_send;
            end if;
        
        when l_s1_send =>
            q.valid <= '1';
            etareg_en_load <= d.ready_rcv;
            elcntd.en <= d.ready_rcv;
            escntd.rst <= '1';
            if elcntq.max = '1' and d.ready_rcv = '1'
            then
                if memcnt < DILITHIUM_l*DILITHIUM_N/4-1
                then
                    nextstate <= l_s1;
                else
                    memcntd.rst <= '1';
                    nextstate <= l_s2;
                end if;
            end if;
        
        ----------------------------------------------------------------------------------------------------
        -- s2
        ----------------------------------------------------------------------------------------------------
        when l_s2 =>
            if escnt <= ESCNT_MAX-GLOBAL_MEMORY_DELAY
            then
                memcntd.en <= '1';
            end if;
            if escnt >= GLOBAL_MEMORY_DELAY-1
            then
                etareg_en_store <= '1';
            end if;
            escntd.en <= '1';
            elcntd.rst <= '1';
            if escntq.max = '1'
            then
                nextstate <= l_s2_send;
            end if;
        
        when l_s2_send =>
            q.valid <= '1';
            etareg_en_load <= d.ready_rcv;
            elcntd.en <= d.ready_rcv;
            escntd.rst <= '1';
            if elcntq.max = '1' and d.ready_rcv = '1'
            then
                if memcntq.ovf = '0'
                then
                    nextstate <= l_s2;
                else
                    memcntd.rst <= '1';
                    nextstate <= l_t0;
                end if;
            end if;
            
            
            
            
            
        when others => nextstate <= idle;
    end case;
end process;

end Behavioral;
