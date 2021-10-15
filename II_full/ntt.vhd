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

entity ntt is
    Port (
        clk :  in std_logic;
        d   :  in ntt_in_type;
        q   : out ntt_out_type
    );
end ntt;

architecture Behavioral of ntt is
    
    constant MEMORY_DELAY : natural := GLOBAL_MEMORY_DELAY;
    constant TWIDDLE_DELAY : natural := 1;
    
    constant BUTTERFLY_DELAY_FWD : integer := 16;
    constant BUTTERFLY_DELAY_INV : integer := 14;
    
    type state_type is (idle, transform, transform_finish, transform_read_done);
    signal state, nextstate : state_type := idle;

    -- butterfly
    signal bfud : double_bfu_in_type;
    signal bfuq : double_bfu_out_type;
    signal bfuidata, bfuodata : payload_array(0 to 3);
    
    -- addresses
    signal nttd, nttwd : ntt_addr_gen_in_type;
    signal nttq, nttwq : ntt_addr_gen_out_type;
    signal nttd_ctrl, nttwd_ctrl : std_logic_vector(1 downto 0);
    
    -- twiddle factors
    type mem_twiddle_d_pipeline_type is array(1 to MEMORY_DELAY-TWIDDLE_DELAY) of mem_twiddle_in_type;
    signal mem_twiddle_d_pipeline : mem_twiddle_d_pipeline_type;
    signal mem_twiddle_d : mem_twiddle_in_type;
    signal mem_twiddle_q : mem_twiddle_out_type;
    signal use_twidb : std_logic := '0';
    
    -- read/write enable
    signal ren,wen : std_logic;
    
    -- register stuff
    signal butterfly_delay : natural range 0 to 63;
    signal waddrgen_delay : natural range 0 to 63;
    signal reg_en : std_logic;

begin

-------------------
-- STATE MACHINE --
-------------------
states: process(clk)
begin
    if rising_edge(clk)
    then
        state <= nextstate;
    end if;
end process;


values: process(state, d.en, nttq.ready, nttwq.ready)
begin
    nextstate <= state;
    
    case state is 
        when idle =>
            -- output
            q.ready <= '1';
            q.ready_read <= '1';
            
            -- internals
            nttd.rst <= '1';
            nttd.en <= '0';
            reg_en <= '0';
            ren <= '0';
            
            -- transition
            if d.en = '1'
            then
                nextstate <= transform;
            end if;
        
        when transform => 
            -- output
            q.ready <= nttwq.ready;
            q.ready_read <= '0';
            
            -- internals
            nttd.rst <= '0';
            nttd.en <= '1';
            reg_en <= '1';
            ren <= '1';
            
            -- transition
            if nttq.ready = '0'
            then
                nextstate <= transform_finish;
            end if;
        
        when transform_finish => 
            -- output
            q.ready <= nttwq.ready;
            q.ready_read <= '0';
            
            -- internals
            nttd.rst <= '0';
            nttd.en <= '1';
            reg_en <= '1';
            ren <= '1';
            
            -- transition
            if nttq.ready = '1'
            then
                ren <= '0';
                nextstate <= transform_read_done;
            end if;
        
        when transform_read_done =>
            -- output
            q.ready <= '0';
            q.ready_read <= '1';
            
            -- internals
            nttd.rst <= '1';
            nttd.en <= '0';
            reg_en <= '1';
            ren <= '0';
            
            -- transitions
            if nttwq.ready = '1'
            then
                nextstate <= idle;
            end if;
            if d.en = '1'
            then
                nextstate <= transform;
            end if;
    end case;
end process;



---------------------
-- TWIDDLE FACTORS --
---------------------
mem_twiddles: entity work.mem_twiddle
port map (
    clk => clk,
    
    d => mem_twiddle_d,
    q => mem_twiddle_q
);

-- delay mem_twiddle_d
assert MEMORY_DELAY >= TWIDDLE_DELAY report "bad delay" severity failure;
twidassign: if MEMORY_DELAY = TWIDDLE_DELAY
generate
    mem_twiddle_d <= nttq.twiddlectrl;
end generate;
twidassign2: if MEMORY_DELAY > TWIDDLE_DELAY
generate
    mem_twiddle_d <= mem_twiddle_d_pipeline(MEMORY_DELAY-TWIDDLE_DELAY);
    delay_mem_twid_b: process(clk)
    begin
        if rising_edge(clk)
        then
            for i in MEMORY_DELAY-TWIDDLE_DELAY downto 2
            loop
                mem_twiddle_d_pipeline(i) <= mem_twiddle_d_pipeline(i-1);
            end loop;
            mem_twiddle_d_pipeline(1) <= nttq.twiddlectrl;
        end if;
    end process;
end generate;

bfud.omega(0) <= mem_twiddle_q(0);
        
delay_mem_twiddle_io_enb: process(clk)
begin
    if rising_edge(clk)
    then
        if reg_en = '1'
        then
            use_twidb <= mem_twiddle_d.en(1);
        end if;
    end if;
end process;

twidmux: process(use_twidb, mem_twiddle_q)
begin
    if use_twidb = '0'
    then
        bfud.omega(1) <= mem_twiddle_q(0);
    else
        bfud.omega(1) <= mem_twiddle_q(1);
    end if;
end process;

-----------------------
---- BUTTERFLY UNITS --
-----------------------
dbfu:  entity work.double_bfu
port map (
    clk => clk,
    d => bfud,
    q => bfuq
);

bfud.en <= reg_en;
bfud.inv <= d.inv;

bfud.a(0) <= d.data(0);
bfud.b(0) <= d.data(1);
bfud.a(1) <= d.data(2);
bfud.b(1) <= d.data(3);

q.wdata(0) <= bfuq.A(0);
q.wdata(1) <= bfuq.B(0);
q.wdata(2) <= bfuq.A(1);
q.wdata(3) <= bfuq.B(1);

---------------------------
-- read and write enable --
---------------------------
wengen: entity work.dyn_shift_reg
generic map (width => 1, max_depth => BUTTERFLY_DELAY_FWD + MEMORY_DELAY)
port map (
    clk => clk,
    ce => '1',--reg_en, -- todo, there were strange errors because of this
    depth => waddrgen_delay,
    d(0) => ren,
    q(0) => wen
);

q.ren <= ren;
q.wen <= wen;

-----------------------
-- Address Generator --
-----------------------
raddrgen: entity work.ntt_addr_gen
port map (
    clk => clk,
    d => nttd,
    q => nttq
);
nttd.inv <= d.inv;

waddrgen: entity work.ntt_addr_gen
port map (
    clk => clk,
    d => nttwd,
    q => nttwq
);
nttwd.inv <= d.inv;
q.raddr <= nttq.addr;
q.waddr <= nttwq.addr;

waddrgen_delay <= BUTTERFLY_DELAY_FWD+MEMORY_DELAY when d.inv='0' else BUTTERFLY_DELAY_INV+MEMORY_DELAY;
ready_delay: entity work.dyn_shift_reg
generic map (width => 2, max_depth => BUTTERFLY_DELAY_FWD + MEMORY_DELAY)
port map (
    clk => clk,
    ce => '1',
    depth => waddrgen_delay,
    d => nttd_ctrl,
    q => nttwd_ctrl
);

nttd_ctrl(1) <= nttd.en;
nttd_ctrl(0) <= nttd.rst;
nttwd.en <= nttwd_ctrl(1);
nttwd.rst <= nttwd_ctrl(0);

end Behavioral;
