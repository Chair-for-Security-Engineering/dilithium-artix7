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

entity keygen is
    Port (
        clk : in std_logic;
        d   : in keygen_in_type;
        q   : out keygen_out_type
    );
end keygen;

architecture Behavioral of keygen is

    constant MEMORY_DELAY : natural := GLOBAL_MEMORY_DELAY;

    type state_type is (idle, 
    expand_seed_start, expand_seed, expand_seed_finish, 
    expand_s1s2, expand_s1s2_finish, 
    expand_A, expand_A_finish, 
    matmul, matmul_finish,
    intt, intt_finish, intt_shortcut,
    add_s2, add_s2_finish, add_s2_shortcut,
    crh_rho_t1, crh_rho_t1_finish);
    signal state, nextstate : state_type := idle;
    
    type maccstate_type is (macc_idle, macc_process, macc_read_done);
    signal maccstate, maccnextstate : maccstate_type := macc_idle;
    
    type nttmux_type is (ntt_matmul, ntt_native);
    signal nttmux : nttmux_type;
    
    type kcnt_type is array(0 to 3) of natural range 0 to DILITHIUM_k-1;
    signal kcnt : kcnt_type;
    signal k_en, k_rst, k_ovf : std_logic_vector(0 to 3);
    
    -- memory mux
    type memmux_type is (memmux_keygen, memmux_expands1s2, memmux_expandA, memmux_matmul, memmux_crhrhot1);
    signal memmux : memmux_type;
    signal memd_keygen : memory_in_type := (others => ZEROMEM);
    signal mem_rw_t0, mem_r_s2, mem_w_t1, mem_ntt_t0 : std_logic; 
    type memm_type is array(0 to 2) of natural range 0 to NUM_MEM_8_POLY-1;
    type pipeline_type is array(1 to MEMORY_DELAY) of memm_type;
    signal pipeline_in, pipeline_out : memm_type;
    signal pipeline : pipeline_type;
    
    -- rho register mux
    type rhomux_type is (rhomux_init, rhomux_expandA, rhomux_crhrhot1);
    signal rhomux : rhomux_type;
    signal rhoregd_init : reg32_in_type;
    
    -- rho prime register mux
    type rhoprimemux_type is (rhoprimemux_init, rhoprimemux_expands1s2);
    signal rhoprimemux : rhoprimemux_type;
    signal rhoprimeregd_init : reg32_in_type;
    
    -- keccak mux
    type keccakmux_type is (keccakmux_init, keccakmux_expandA, keccakmux_expands1s2, keccakmux_crhrhot1);
    signal keccakmux : keccakmux_type;
    signal keccakd_init : keccak_in_type;
    
    -- seed expansion
    signal seedexpcntd : counter_in_type;
    signal seedexpcntq : counter_out_type;
    signal seedexpcnt : natural range 0 to SHAKE128_RATE/32 + 24 + 256/32 + 512/32 + 256/32 + 1;
    
    -- power2round
    signal p2rlow, p2rhigh : payload_array(0 to 3);
        
begin

------------------------------------------------------------------------------------
-- memory mux
------------------------------------------------------------------------------------
with memmux
select
    q.memd <= memd_keygen when memmux_keygen,
              d.expands1s2q.memd when memmux_expands1s2,
              d.expandAq.memd when memmux_expandA,
              d.matmulq.memd when memmux_matmul,
              d.crtq.memd when memmux_crhrhot1;

------------------------------------------------------------------------------------
-- rho prime register input handling
------------------------------------------------------------------------------------
with rhoprimemux
select
    q.rhoprimeregd <= rhoprimeregd_init when rhoprimemux_init,
                    d.expands1s2q.seedregd when rhoprimemux_expands1s2;
                    
------------------------------------------------------------------------------------
-- rho register input handling
------------------------------------------------------------------------------------
with rhomux
select
    q.rhoregd <=    rhoregd_init when rhomux_init,
                    d.expandAq.seedregd when rhomux_expandA,
                    d.crtq.rhoregd when rhomux_crhrhot1;

------------------------------------------------------------------------------------
-- keccak input handling
------------------------------------------------------------------------------------
with keccakmux
select
    q.keccakd <=    keccakd_init when keccakmux_init,
                    d.expandAq.keccakd when keccakmux_expandA,
                    d.expands1s2q.keccakd when keccakmux_expands1s2,
                    d.crtq.keccakd when keccakmux_crhrhot1;

------------------------------------------------------------------------------------
-- expand A
------------------------------------------------------------------------------------
q.expandAd.seedregq <= d.rhoregq;
q.expandAd.keccakq <= d.keccakq;

------------------------------------------------------------------------------------
-- expand s1 and s2
------------------------------------------------------------------------------------
q.expands1s2d.seedregq <= d.rhoprimeregq;
q.expands1s2d.keccakq <= d.keccakq;

------------------------------------------------------------------------------------
-- matmul, macc, ntt
------------------------------------------------------------------------------------
q.matmuld.nttq <= d.nttq;
q.matmuld.maccq <= d.maccq;
q.matmuld.memq <= d.memq;



------------------------------------------------------------------------------------
-- ntt and macc mux
------------------------------------------------------------------------------------
mux: process(nttmux, d.memq, d.matmulq.nttd.data, d.matmulq.maccd, pipeline_out)
begin
    if nttmux = ntt_matmul
    then
        q.nttd.data <= d.matmulq.nttd.data;
        q.maccd.memq_a <= d.matmulq.maccd.memq_a;
        q.maccd.memq_b <= d.matmulq.maccd.memq_b;
        q.maccd.memq_c <= d.matmulq.maccd.memq_c;
    else
        q.nttd.data <= (others => (others => '0'));
        q.maccd.memq_a <= (others => (others => '0'));
        q.maccd.memq_b <= (others => (others => '0'));
        q.maccd.memq_c <= (others => (others => '0'));
        for m in 0 to NUM_MEM_8_POLY-1
        loop
            if m = pipeline_out(0)
            then
                q.nttd.data <= d.memq(m);
            end if;
            if m = pipeline_out(1)
            then
                q.maccd.memq_a <= d.memq(m);
            end if;
            if m = pipeline_out(2)
            then
                q.maccd.memq_b <= d.memq(m);
            end if;
        end loop;
    end if;
end process;

pipeline_out <= pipeline(MEMORY_DELAY);
memdelay: process(clk)
begin
    if rising_edge(clk)
    then
        for i in MEMORY_DELAY downto 2
        loop
            pipeline(i) <= pipeline(i-1);
        end loop;
        pipeline(1) <= pipeline_in;
    end if;
end process;

pipeline_in(0) <= memory_map.t0(kcnt(0)).memory_index;
pipeline_in(1) <= memory_map.t0(kcnt(2)).memory_index;
pipeline_in(2) <= memory_map.s2(kcnt(2)).memory_index;

------------------------------------------------------------------------------------
-- memd_keygen mux
------------------------------------------------------------------------------------
mkmux: process(kcnt, d.maccq, d.nttq, p2rlow, p2rhigh, mem_rw_t0, mem_r_s2, mem_w_t1, mem_ntt_t0)
begin
    for m in 0 to NUM_MEM_8_POLY-1
    loop
        -- initialize
        memd_keygen(m) <= ZEROMEM;
    
        if m = memory_map.t0(kcnt(2)).memory_index and mem_rw_t0 = '1'
        then
            memd_keygen(m).rsel <= memory_map.t0(kcnt(2)).poly_index;
            memd_keygen(m).raddr <= d.maccq.raddr;
            memd_keygen(m).ren <= (others => d.maccq.ren);
            memd_keygen(m).wsel <= memory_map.t0(kcnt(2)).poly_index;
            memd_keygen(m).waddr <= d.maccq.waddr;
            memd_keygen(m).wen <= (others => d.maccq.wen);
            memd_keygen(m).wdata <= p2rlow;
        end if;
    
        if m = memory_map.s2(kcnt(2)).memory_index and mem_r_s2 = '1'
        then
            memd_keygen(m).rsel <= memory_map.s2(kcnt(2)).poly_index;
            memd_keygen(m).raddr <= d.maccq.raddr;
            memd_keygen(m).ren <= (others => d.maccq.ren);
        end if;
    
        if m = memory_map.t1(kcnt(2)).memory_index and mem_w_t1 = '1'
        then
            memd_keygen(m).wsel <= memory_map.t1(kcnt(2)).poly_index;
            memd_keygen(m).waddr <= d.maccq.waddr;
            memd_keygen(m).wen <= (others => d.maccq.wen);
            memd_keygen(m).wdata <= p2rhigh;
        end if;
        
        if m = memory_map.t0(kcnt(1)).memory_index and mem_ntt_t0 = '1'
        then
            memd_keygen(m).wsel <= memory_map.t0(kcnt(1)).poly_index;
            memd_keygen(m).waddr <= d.nttq.waddr;
            memd_keygen(m).wen <= (others => d.nttq.wen);
            memd_keygen(m).wdata <= d.nttq.wdata;
        end if;
        
        if m = memory_map.t0(kcnt(0)).memory_index and mem_ntt_t0 = '1'
        then
            memd_keygen(m).rsel <= memory_map.t0(kcnt(0)).poly_index;
            memd_keygen(m).raddr <= d.nttq.raddr;
            memd_keygen(m).ren <= (others => d.nttq.ren);
        end if;
        
    end loop;
end process;

power2round: for i in 0 to 3
generate
    p2rlow(i) <= std_logic_vector
    (
        to_unsigned
        (
            to_integer
            (
                signed
                (
                    d.maccq.wdata(i)(DILITHIUM_d-1 downto 0)
                )
            )+DILITHIUM_Q,
            23
        )
    ) when unsigned(d.maccq.wdata(i)(DILITHIUM_d-1 downto 0)) > (2**(DILITHIUM_d-1)) else "0000000000" & d.maccq.wdata(i)(DILITHIUM_d-1 downto 0);
    p2rhigh(i) <= std_logic_vector
    (
        to_unsigned
        (
            to_integer
            (
                unsigned
                (
                    d.maccq.wdata(i)(22 downto DILITHIUM_d)
                )
            ) + 1,
            10
        )
    ) & "0000000000000" when unsigned(d.maccq.wdata(i)(DILITHIUM_d-1 downto 0)) > (2**(DILITHIUM_d-1)) else d.maccq.wdata(i)(22 downto DILITHIUM_d) & "0000000000000";
end generate;

------------------------------------------------------------------------------------
-- crh rho t1
------------------------------------------------------------------------------------
q.crtd.keccakq <= d.keccakq;
q.crtd.rhoregq <= d.rhoregq;
q.crtd.memq <= d.memq;
q.trregd <= d.crtq.trregd;

------------------------------------------------------------------------------------
-- intt k counter
------------------------------------------------------------------------------------
cntr: process(clk)
begin
    if rising_edge(clk)
    then
        for i in 0 to 3
        loop
            if k_rst(i) = '1'
            then
                kcnt(i) <= 0;
                k_ovf(i) <= '0';
            elsif k_en(i) = '1' and k_ovf(i) = '0'
            then
                kcnt(i) <= kcnt(i) + 1;
                if kcnt(i) = DILITHIUM_k-1
                then
                    kcnt(i) <= 0;
                    k_ovf(i) <= '1';
                end if;
            end if;
        end loop;
    end if;
end process;


------------------------------------------------------------------------------------
-- seed expansion ctrl
------------------------------------------------------------------------------------
seedexp_counter: entity work.counter
generic map (max_value => SHAKE128_RATE/32 + 24 + 256/32 + 512/32 + 256/32 + 1)
port map (
    clk => clk,
    d => seedexpcntd,
    q => seedexpcntq,
    value => seedexpcnt
);

q.seedregd.en_write <= '0';

seedexpmux: process(seedexpcnt, seedexpcntd, d.seedregq.data, d.keccakq.data)
begin

    keccakd_init.data <= (others => '0');
    q.seedregd.en_rotate <= '0';
    rhoregd_init.en_rotate <= '0';
    rhoregd_init.en_write <= '0';
    rhoregd_init.data <= (others => '0');
    rhoprimeregd_init.en_rotate <= '0';
    rhoprimeregd_init.en_write <= '0';
    rhoprimeregd_init.data <= (others => '0');
    q.Kregd.en_rotate <= '0';
    q.Kregd.en_write <= '0';
    q.Kregd.data <= (others => '0');
    
    case seedexpcnt is 
        when 0 => 
            keccakd_init.data <= d.seedregq.data;
        
        when 1 to 8 =>
            keccakd_init.data <= d.seedregq.data;
            q.seedregd.en_rotate <= '1';
        
        when 9 =>
            keccakd_init.data <= x"1f000000";
        
        when 34 => -- shake 256
            keccakd_init.data <= x"00000080";
            
        when 43 to 66 => -- permutation
            
        when 67 to 74 => -- squeeze: rho
            rhoregd_init.en_rotate <= '1';
            rhoregd_init.en_write <= '1';
            rhoregd_init.data <= d.keccakq.data;
            
        when 75 to 90 => -- squeeze: rhoprime
            rhoprimeregd_init.en_rotate <= '1';
            rhoprimeregd_init.en_write <= '1';
            rhoprimeregd_init.data <= d.keccakq.data;
            
        when 91 to 98 => -- squeeze: K
            q.Kregd.en_rotate <= '1';
            q.Kregd.en_write <= '1';
            q.Kregd.data <= d.keccakq.data;
            
        when 99 => -- last
        
        when others =>
    end case;
end process;



------------------------------------------------------------------------------------
-- state machine
------------------------------------------------------------------------------------
states: process(clk)
begin
    if rising_edge(clk)
    then
        state <= nextstate;
    end if;
end process;

ctrl: process(state, d,
        seedexpcntq.max, -- seed expansion
        d.expands1s2q.ready, -- expand s1 s2
        d.expandAq.ready, -- expand A
        d.matmulq.ready, d.matmulq.maccd.en, -- matmul
        d.nttq.ready, d.nttq.ready_read, d.maccq.ready, k_ovf, -- inverse ntt of product with addition of s2 and power2round
        d.crtq.ready) -- hash rho and t1
begin
    nextstate <= state;
    
    
    -- STANDARD VALUES (idle)
    -- output signals
    q.ready <= '0';
    
    -- muxes
    memmux <= memmux_keygen;
    rhomux <= rhomux_init;
    rhoprimemux <= rhoprimemux_init;
    keccakmux <= keccakmux_init;
    nttmux <= ntt_matmul;
    
    -- memory
    mem_ntt_t0 <= '0';
    mem_rw_t0 <= '0';
    mem_r_s2 <= '0';
    mem_w_t1 <= '0';
    
    -- seed expansion
    seedexpcntd.en <= '0';
    seedexpcntd.rst <= '1';
    keccakd_init.en <= '0';
    keccakd_init.rst <= '1';
    
    -- expand ctrl
    q.expands1s2d.en <= '0';
    q.expandAd.en <= '0';
    
    -- matmul ctrl
    q.matmuld.en <= '0';
    q.matmuld.vecaddr <= none;
    q.matmuld.resvecaddr <= none;
    
    -- ntt ctrl -- per default driven by matmul module
    q.nttd.en <= d.matmulq.nttd.en;
    q.nttd.inv <= d.matmulq.nttd.inv;
    k_en <= "0000";
    k_rst <= "1111";
    
    -- macc ctrl -- per default driven by matmul module
    q.maccd.en <= d.matmulq.maccd.en;
    q.maccd.rst <= d.matmulq.maccd.rst;
    q.maccd.op <= op_macc;
    
    -- crh rho t1 crtl
    q.crtd.en <= '0';
    
    case state is
        when idle =>
            q.ready <= '1';
            
            q.maccd.rst <= '1';
            
            -- transition
            if d.en = '1' and d.keccakq.ready = '1'
            then
                nextstate <= expand_seed;
            end if;
        
        -------------------------------- seed expansion
        when expand_seed =>
            seedexpcntd.en <= '1';
            seedexpcntd.rst <= '0';
            keccakd_init.en <= '1';
            keccakd_init.rst <= '0';
            
            -- transition
            if seedexpcntq.max = '1'
            then
                nextstate <= expand_s1s2;
                report "expands1s2 start";
            end if;
            
        -------------------------------- s1 s2 expansion
        when expand_s1s2 =>
            q.expands1s2d.en <= '1';
            memmux <= memmux_expands1s2;
            rhoprimemux <= rhoprimemux_expands1s2;
            keccakmux <= keccakmux_expands1s2;
            
            -- transition
            if d.expands1s2q.ready = '0'
            then
                nextstate <= expand_s1s2_finish;
            end if;
            
        when expand_s1s2_finish =>
            q.expands1s2d.en <= '0';
            memmux <= memmux_expands1s2;
            rhoprimemux <= rhoprimemux_expands1s2;
            keccakmux <= keccakmux_expands1s2;
            
            -- transition
            if d.expands1s2q.ready = '1' -- done
            then
                report "expands1s2 stop";
                report "expandA start";
                nextstate <= expand_A;
            end if;
            
        -------------------------------- A expansion
        when expand_A =>
            q.expandAd.en <= '1';
            memmux <= memmux_expandA;
            rhomux <= rhomux_expandA;
            keccakmux <= keccakmux_expandA;
            
            -- transition
            if d.expandAq.ready = '0'
            then
                nextstate <= expand_A_finish;
            end if;
            
        when expand_A_finish =>
            q.expandAd.en <= '0';
            memmux <= memmux_expandA;
            rhomux <= rhomux_expandA;
            keccakmux <= keccakmux_expandA;
            
            -- transition
            if d.expandAq.ready = '1'
            then
                report "expandA stop";
                nextstate <= matmul;
            end if;
            
        -------------------------------- multiplication A * s1 =: t0
        when matmul =>
            q.matmuld.en <= '1';
            memmux <= memmux_matmul;
            q.matmuld.vecaddr <= t1;
            q.matmuld.resvecaddr <= t0;
            
            -- transition
            if d.matmulq.ready = '0' -- started
            then
                nextstate <= matmul_finish;
            end if;
            
        when matmul_finish =>
            q.matmuld.en <= '0';
            memmux <= memmux_matmul;
            q.matmuld.vecaddr <= t1;
            q.matmuld.resvecaddr <= t0;
            
            -- transition
            if d.matmulq.ready = '1' -- done
            then
                nextstate <= intt;
            end if;
        
        -------------------------------- inverse NTT of t0, pipelined with addition of s2, then store to t0, t1 (power2round on the fly)
        when intt =>
            q.nttd.en <= '1';
            q.nttd.inv <= '1';
            nttmux <= ntt_native;
            mem_ntt_t0 <= '1';
            k_en <= "0000";
            k_rst <= "0000";
            
            -- transition
            if d.nttq.ready = '0' -- started
            then
                nextstate <= intt_finish;
            end if;
            
        when add_s2 =>
            q.nttd.en <= '0';
            q.nttd.inv <= '1';
            nttmux <= ntt_native;
            if k_ovf(1) = '0'
            then
                mem_ntt_t0 <= '1';
            end if;
            k_en <= "0000";
            k_rst <= "0000";
            
            -- macc ctrl:
            q.maccd.en <= '1';
            q.maccd.rst <= '0';
            q.maccd.op <= op_add;
            mem_rw_t0 <= '1';
            mem_r_s2 <= '1';
            mem_w_t1 <= '1';
            
            -- transition
            if d.maccq.ready = '0' -- started
            then
                nextstate <= add_s2_finish;
            end if;
            
        when add_s2_finish =>
            q.nttd.en <= '0';
            q.nttd.inv <= '1';
            nttmux <= ntt_native;
            if k_ovf(1) = '0'
            then
                mem_ntt_t0 <= '1';
            end if;
            k_en <= "0000";
            k_rst <= "0000";
            
            -- macc ctrl:
            q.maccd.en <= '0';
            q.maccd.rst <= '0';
            q.maccd.op <= op_add;
            mem_rw_t0 <= '1';
            mem_r_s2 <= '1';
            mem_w_t1 <= '1';
            
            -- transition
            if d.maccq.ready = '1' -- ready to add next polynomial
            then
                k_en(2) <= '1';
                if k_ovf(1) = '1'
                then
                    report "crhrhot1 start";
                    nextstate <= crh_rho_t1;
                else
                    nextstate <= intt_finish;
                end if;
            end if;
        
        when intt_finish =>
            q.nttd.en <= '0';
            q.nttd.inv <= '1';
            nttmux <= ntt_native;
            mem_ntt_t0 <= '1';
            k_en <= "0000";
            k_rst <= "0000";
            
            -- macc ctrl:
            q.maccd.en <= '0';
            q.maccd.rst <= '1';
            q.maccd.op <= op_add;
            
            -- transition
            if d.nttq.ready_read = '1' -- ready to transform the next polynomial
            then
                k_en(0) <= '1'; -- read next t0
                nextstate <= intt_shortcut;
            end if;
        
        when intt_shortcut =>
            q.nttd.en <= not k_ovf(0);
            q.nttd.inv <= '1';
            nttmux <= ntt_native;
            mem_ntt_t0 <= '1';
            k_en <= "0000";
            k_rst <= "0000";
            
            -- macc ctrl:
            q.maccd.en <= '0';
            q.maccd.op <= op_add;
            
            -- transition
            if d.nttq.ready = '1' -- ready to transform the next polynomial
            then
                k_en(1) <= '1'; -- write next t0
                nextstate <= add_s2;
            end if;
        
        
        
        -------------------------------- hash rho and t1
        when crh_rho_t1 =>
            q.crtd.en <= '1';
            memmux <= memmux_crhrhot1;
            rhomux <= rhomux_crhrhot1;
            keccakmux <= keccakmux_crhrhot1;
            
            -- transition
            if d.crtq.ready = '0' 
            then
                nextstate <= crh_rho_t1_finish;
            end if;
        
        when crh_rho_t1_finish =>
            q.crtd.en <= '1';
            memmux <= memmux_crhrhot1;
            rhomux <= rhomux_crhrhot1;
            keccakmux <= keccakmux_crhrhot1;
            
            if d.crtq.ready = '1' 
            then
                report "crhrhot1 stop";
                nextstate <= idle; -- DONE
            end if;
        
            
        
        when others => nextstate <= idle;
    end case;
end process;

end Behavioral;
