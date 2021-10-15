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

entity verify is
    Port (
        clk : in std_logic;
        d   : in verify_in_type;
        q   : out verify_out_type
    );
end verify;

architecture Behavioral of verify is

    type state_type is (idle, 
    check_z_start, 
    ballsample, ballsample_start, ballsample_finish, 
    nttc, nttc_finish, 
    matmul, matmul_finish, 
    mul_c, mul_c_finish, mul_c_shortcut,
    sub_from_Az, sub_from_Az_finish, sub_from_Az_shortcut,
    invntt, invntt_finish, invntt_shortcut,
    use_hint, use_hint_finish,
    result_good, result_bad);
    signal state, nextstate : state_type;
    
    signal nttd_vrfy : ntt_in_type;
    signal maccd_vrfy : macc_poly_in_type;
    
    signal memd_vrfy : memory_in_type;
    
    signal kcntd, kmacccntd : counter_in_type;
    signal kcntq, kmacccntq : counter_out_type;
    signal kcnt, kmacccnt : natural range 0 to DILITHIUM_k-1;
    
    type memi_type is array(0 to 2) of natural range 0 to NUM_MEM_8_POLY-1;
    type memi_pipeline_type is array(1 to GLOBAL_MEMORY_DELAY) of memi_type;
    signal memi, memi_delayed : memi_type := (0,0,0);
    signal memi_pipeline : memi_pipeline_type := (others => (0,0,0));

begin
    ----------------------------------------------------------------------------------------------------
    -- memory muxing
    ----------------------------------------------------------------------------------------------------
    with state
    select
        q.memd <= d.matmulq.memd when matmul | matmul_finish,
                  d.usehintq.memd when use_hint | use_hint_finish,
                  memd_vrfy when others;
    midelay: process(clk)
    begin
        if rising_edge(clk)
        then
            for i in GLOBAL_MEMORY_DELAY downto 2
            loop
                memi_pipeline(i) <= memi_pipeline(i-1);
            end loop;
            memi_pipeline(1) <= memi;
        end if;
    end process;
    memi_delayed <= memi_pipeline(GLOBAL_MEMORY_DELAY);
    mvmux: process(state, d, kcnt, kcntq, memi_delayed)
    variable mintt,mimca,mimcb,pintt,pimca,pimcb,mimcr,pimcr : natural;
    begin
        memd_vrfy <= (others => ZEROMEM);
        nttd_vrfy.data <= (others => (others => '0'));
        maccd_vrfy.memq_a <= (others => (others => '0'));
        maccd_vrfy.memq_b <= (others => (others => '0'));
        maccd_vrfy.memq_c <= (others => (others => '0'));
        memi <= (0,0,0);
        mintt := 0;
        mimca := 0;
        mimcb := 0;
        pintt := 0;
        pimca := 0;
        pimcb := 0;
        mimcr := 0;
        pimcr := 0;
        case state is
            when check_z_start | ballsample | ballsample_start | ballsample_finish =>
                -- special case! ballsample writes only, check_z reads only
                for m in 0 to NUM_MEM_8_POLY-1
                loop
                    memd_vrfy(m).ren <= d.chkzq.memd(m).ren;
                    memd_vrfy(m).raddr <= d.chkzq.memd(m).raddr;
                    memd_vrfy(m).rsel <= d.chkzq.memd(m).rsel;
                    memd_vrfy(m).wen <= d.ballsampleq.memd(m).wen;
                    memd_vrfy(m).waddr <= d.ballsampleq.memd(m).waddr;
                    memd_vrfy(m).wsel <= d.ballsampleq.memd(m).wsel;
                    memd_vrfy(m).wdata <= d.ballsampleq.memd(m).wdata;
                end loop;
        
            when nttc | nttc_finish =>
                memd_vrfy(memory_map.c.memory_index).rsel <= memory_map.c.poly_index; 
                memd_vrfy(memory_map.c.memory_index).wsel <= memory_map.c.poly_index;
                memd_vrfy(memory_map.c.memory_index).ren <= (others => d.nttq.ren);
                memd_vrfy(memory_map.c.memory_index).wen <= (others => d.nttq.wen);
                memd_vrfy(memory_map.c.memory_index).raddr <= d.nttq.raddr;
                memd_vrfy(memory_map.c.memory_index).waddr <= d.nttq.waddr;
                memd_vrfy(memory_map.c.memory_index).wdata <= d.nttq.wdata;
                nttd_vrfy.data <= d.memq(memory_map.c.memory_index);
            
            when mul_c | mul_c_finish | mul_c_shortcut => 
                mimca := memory_map.c.memory_index;
                mimcb := memory_map.t1(kcnt).memory_index;
                pimcb := memory_map.t1(kcnt).poly_index;
                mimcr := memory_map.tmp0(kcnt).memory_index;
                pimcr := memory_map.tmp0(kcnt).poly_index;
                memd_vrfy(mimca).rsel <= memory_map.c.poly_index;
                memd_vrfy(mimca).raddr <= d.maccq.raddr;
                memd_vrfy(mimca).ren <= (others => d.maccq.ren);
                memd_vrfy(mimcb).rsel <= pimcb;
                memd_vrfy(mimcb).raddr <= d.maccq.raddr;
                memd_vrfy(mimcb).ren <= (others => d.maccq.ren);
                if state = mul_c_shortcut and kcnt > 0
                then
                    mimcr := memory_map.tmp0(kcnt-1).memory_index;
                    pimcr := memory_map.tmp0(kcnt-1).poly_index;
                end if;
                memd_vrfy(mimcr).wsel <= pimcr;
                memd_vrfy(mimcr).waddr <= d.maccq.waddr;
                memd_vrfy(mimcr).wen <= (others => d.maccq.wen);
                memd_vrfy(mimcr).wdata <= d.maccq.wdata;
                
                maccd_vrfy.memq_a <= d.memq(memi_delayed(1));
                maccd_vrfy.memq_b <= d.memq(memi_delayed(2));
            
            when sub_from_Az | sub_from_Az_finish | sub_from_Az_shortcut => 
                mimca := memory_map.tmp0(kcnt).memory_index;
                pimca := memory_map.tmp0(kcnt).poly_index;
                mimcb := memory_map.w(kcnt).memory_index;
                pimcb := memory_map.w(kcnt).poly_index;
                mimcr := memory_map.w(kcnt).memory_index;
                pimcr := memory_map.w(kcnt).poly_index;
                memd_vrfy(mimca).rsel <= pimca;
                memd_vrfy(mimca).raddr <= d.maccq.raddr;
                memd_vrfy(mimca).ren <= (others => d.maccq.ren);
                memd_vrfy(mimcb).rsel <= pimcb;
                memd_vrfy(mimcb).raddr <= d.maccq.raddr;
                memd_vrfy(mimcb).ren <= (others => d.maccq.ren);
                if state = sub_from_Az_shortcut and kcnt > 0
                then
                    mimcr := memory_map.w(kcnt-1).memory_index;
                    pimcr := memory_map.w(kcnt-1).poly_index;
                end if;
                memd_vrfy(mimcr).wsel <= pimcr;
                memd_vrfy(mimcr).waddr <= d.maccq.waddr;
                memd_vrfy(mimcr).wen <= (others => d.maccq.wen);
                memd_vrfy(mimcr).wdata <= d.maccq.wdata;
                
                maccd_vrfy.memq_a <= d.memq(memi_delayed(1));
                maccd_vrfy.memq_b <= d.memq(memi_delayed(2));
            
            when invntt | invntt_finish | invntt_shortcut =>
                mintt := memory_map.w(kcnt).memory_index;
                pintt := memory_map.w(kcnt).poly_index;
                if state = invntt_shortcut and kcnt > 0 and kcntq.ovf = '0'
                then
                    mintt := memory_map.w(kcnt-1).memory_index;
                    pintt := memory_map.w(kcnt-1).poly_index;
                end if;
                memd_vrfy(mintt).wsel <= pintt;
                memd_vrfy(mintt).waddr <= d.nttq.waddr;
                memd_vrfy(mintt).wdata <= d.nttq.wdata;
                memd_vrfy(mintt).wen <= (others => d.nttq.wen);
                mintt := memory_map.w(kcnt).memory_index;
                memd_vrfy(mintt).rsel <= memory_map.w(kcnt).poly_index;
                memd_vrfy(mintt).raddr <= d.nttq.raddr;
                memd_vrfy(mintt).ren <= (others => d.nttq.ren);
                nttd_vrfy.data <= d.memq(memi_delayed(0));
            when others =>
        end case;
        
        memi(0) <= mintt;
        memi(1) <= mimca;
        memi(2) <= mimcb;
    end process;
    
    ----------------------------------------------------------------------------------------------------
    -- use_hint
    ----------------------------------------------------------------------------------------------------
    q.usehintd.memq <= d.memq;
    q.usehintd.keccakq <= d.keccakq;
    q.usehintd.muregq <= d.muregq;
    q.usehintd.chashregq <= d.chashregq;
    q.usehintd.hregq <= d.hregq;
    
    q.muregd <= d.usehintq.muregd;
    q.hregd <= d.usehintq.hregd;
    
    ----------------------------------------------------------------------------------------------------
    -- ballsample and use_hint muxing
    ----------------------------------------------------------------------------------------------------
    with state 
    select
        q.keccakd <= d.usehintq.keccakd when use_hint | use_hint_finish,
                     d.ballsampleq.keccakd when others;
    with state
    select
        q.chashregd <=  d.usehintq.chashregd when use_hint | use_hint_finish,
                        d.ballsampleq.seedregd when others;
    q.ballsampled.seedregq <= d.chashregq;
    q.ballsampled.keccakq <= d.keccakq;
    
    ----------------------------------------------------------------------------------------------------
    -- ntt and macc mux
    ----------------------------------------------------------------------------------------------------
    q.nttd <= d.matmulq.nttd when state = matmul or state = matmul_finish else nttd_vrfy;
    q.maccd <= d.matmulq.maccd when state = matmul or state = matmul_finish else maccd_vrfy;
    
    ----------------------------------------------------------------------------------------------------
    -- matmul handling
    ----------------------------------------------------------------------------------------------------
    q.matmuld.maccq <= d.maccq;
    q.matmuld.nttq <= d.nttq;
    q.matmuld.memq <= d.memq;

    ----------------------------------------------------------------------------------------------------
    -- k counter
    ----------------------------------------------------------------------------------------------------
    k_counter: entity work.counter
    generic map (max_value => DILITHIUM_k-1)
    port map (
        clk => clk,
        d => kcntd,
        q => kcntq,
        value => kcnt
    );

    ----------------------------------------------------------------------------------------------------
    -- state machine
    ----------------------------------------------------------------------------------------------------
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
    
    signals: process(state, d, kcntq)
    begin
        
        nextstate <= state;
        
        q.ready <= '0';
        q.valid <= '0';
        q.result <= '0';
        
        q.chkzd.en <= '0';
        q.chkzd.rst <= '0';
        q.chkzd.ct0 <= '0';
        
        q.ballsampled.rst <= '0';
        q.ballsampled.en <= '0';
        
        nttd_vrfy.en <= '0';
        nttd_vrfy.inv <= '0';
        
        maccd_vrfy.en <= '0';
        maccd_vrfy.rst <= '0';
        maccd_vrfy.op <= op_macc;
        
        kcntd.en <= '0';
        kcntd.rst <= '0';
        
        q.matmuld.en <= '0';
        q.matmuld.vecaddr <= z;
        q.matmuld.resvecaddr <= w;
        
        q.usehintd.en <= '0';
        q.usehintd.nohint_writechash <= '0';
        q.usehintd.rst <= '0';
        
        case state is
            when idle =>
                q.ready <= '1';
                
                kcntd.rst <= '1';
                maccd_vrfy.rst <= '1';
                q.chkzd.rst <= '1';
                q.ballsampled.rst <= '1';
                q.usehintd.rst <= '1';
                
                if d.en = '1' and d.chkzq.ready = '1'
                then
                    nextstate <= check_z_start;
                end if;
            
            -----------------------------------------------------------------------------------
            -- start checking hamming weight of q
            -----------------------------------------------------------------------------------
            when check_z_start =>
                q.chkzd.en <= '1';
                
                if d.chkzq.ready = '0'
                then
                    nextstate <= ballsample;
                    report "ballsample start";
                end if;
            
            -----------------------------------------------------------------------------------
            -- sample to a ball
            -----------------------------------------------------------------------------------
            when ballsample =>
                q.ballsampled.rst <= '1';
                
                if d.ballsampleq.ready = '1'
                then
                    nextstate <= ballsample_start;
                end if;
                if d.chkzq.valid = '1' and d.chkzq.result_bad = '1' -- break, z is bad
                then
                    nextstate <= result_bad;
                end if;
            
            when ballsample_start =>
                q.ballsampled.en <= '1';
                if d.ballsampleq.ready = '0'
                then
                    nextstate <= ballsample_finish;
                end if;
                if d.chkzq.valid = '1' and d.chkzq.result_bad = '1' -- break, z is bad
                then
                    nextstate <= result_bad;
                end if;
            
            when ballsample_finish =>
                q.ballsampled.en <= '0';
                if d.ballsampleq.ready = '1' and d.chkzq.ready = '1' -- wait for z checking
                then
                    nextstate <= nttc;
                    report "ballsample stop";
                end if;
                if d.chkzq.valid = '1' and d.chkzq.result_bad = '1' -- break, z is bad
                then
                    nextstate <= result_bad;
                end if;
            
            -----------------------------------------------------------------------------------
            -- ntt of c
            -----------------------------------------------------------------------------------
            when nttc =>
                nttd_vrfy.en <= '1';
                nttd_vrfy.inv <= '0';
                
                if d.nttq.ready = '0'
                then
                    nextstate <= nttc_finish;
                end if;
            
            when nttc_finish =>
                nttd_vrfy.en <= '0';
                nttd_vrfy.inv <= '0';
                
                if d.nttq.ready = '1'
                then
                    nextstate <= matmul;
                end if;
            
            -----------------------------------------------------------------------------------
            -- matmul A*z=:w
            -----------------------------------------------------------------------------------
            when matmul =>
                q.matmuld.en <= '1';
                
                if d.matmulq.ready = '0' -- started
                then
                    nextstate <= matmul_finish;
                end if;
                
            when matmul_finish =>
                q.matmuld.en <= '0';
                
                if d.matmulq.ready = '1' -- finished
                then
                    nextstate <= mul_c;
                end if;
                
            -----------------------------------------------------------------------------------
            -- multiply c with  t1
            -----------------------------------------------------------------------------------
            when mul_c =>
                maccd_vrfy.en <= '1';
                maccd_vrfy.op <= op_macc;
                if d.maccq.ready = '0'
                then
                    nextstate <= mul_c_finish;
                end if;
                
            when mul_c_finish =>
                maccd_vrfy.en <= '0';
                maccd_vrfy.op <= op_macc;
                if kcntq.max = '1' and d.maccq.ready = '1'
                then
                    kcntd.rst <= '1';
                    nextstate <= sub_from_Az;
                elsif kcntq.max = '0' and d.maccq.ready_read = '1'
                then
                    kcntd.en <= '1';
                    nextstate <= mul_c_shortcut;
                end if;
                
            when mul_c_shortcut =>
                maccd_vrfy.en <= '1';
                maccd_vrfy.op <= op_macc;
                
                if d.maccq.ready = '1'
                then
                    nextstate <= mul_c_finish;
                end if;
                
            -----------------------------------------------------------------------------------
            -- subtract c*t1 from Az
            -----------------------------------------------------------------------------------
            when sub_from_Az =>
                maccd_vrfy.en <= '1';
                maccd_vrfy.op <= op_sub;
                if d.maccq.ready = '0'
                then
                    nextstate <= sub_from_Az_finish;
                end if;
                
            when sub_from_Az_finish =>
                maccd_vrfy.en <= '0';
                maccd_vrfy.op <= op_sub;
                if kcntq.max = '1' and d.maccq.ready = '1'
                then
                    kcntd.rst <= '1';
                    nextstate <= invntt;
                elsif kcntq.max = '0' and d.maccq.ready_read = '1'
                then
                    kcntd.en <= '1';
                    nextstate <= sub_from_Az_shortcut;
                end if;
                
            when sub_from_Az_shortcut =>
                maccd_vrfy.en <= '1';
                maccd_vrfy.op <= op_sub;
                if d.maccq.ready = '1'
                then
                    nextstate <= sub_from_Az_finish;
                end if;
                
            -----------------------------------------------------------------------------------
            -- inverse ntt of result
            -----------------------------------------------------------------------------------
            when invntt =>
                nttd_vrfy.en <= '1';
                nttd_vrfy.inv <= '1';
                
                if d.nttq.ready_read = '0'
                then
                    nextstate <= invntt_finish;
                end if;
                
            when invntt_finish =>
                nttd_vrfy.en <= '0';
                nttd_vrfy.inv <= '1';
                
                if d.nttq.ready_read = '1'
                then
                    kcntd.en <= '1';
                    nextstate <= invntt_shortcut;
                end if;
                
            when invntt_shortcut =>
                nttd_vrfy.inv <= '1';
                if kcntq.ovf = '0'
                then
                    nttd_vrfy.en <= '1';
                end if;
                
                if d.nttq.ready = '1'
                then
                    if kcntq.ovf = '1'
                    then
                        nextstate <= use_hint;
                        report "usehint start";
                    else
                        nextstate <= invntt_finish;
                    end if;
                end if;
                    
            
            -----------------------------------------------------------------------------------
            -- use hint
            -----------------------------------------------------------------------------------
            when use_hint =>
                q.usehintd.en <= '1';
                
                if d.usehintq.ready = '0'
                then
                    nextstate <= use_hint_finish;
                end if;
                
            when use_hint_finish =>
                q.usehintd.en <= '0';
                
                if d.usehintq.valid = '1'
                then
                    if d.usehintq.result = '1'
                    then
                        nextstate <= result_good;
                        report "usehint stop good";
                    else
                        nextstate <= result_bad;
                        report "usehint stop bad";
                    end if;
                end if;
            
            -----------------------------------------------------------------------------------
            -- return result
            -----------------------------------------------------------------------------------
            when result_good =>
                q.ready <= '1';
                q.valid <= '1';
                q.result <= '1';
                
            when result_bad =>
                q.ready <= '1';
                q.valid <= '1';
                q.result <= '0';
                
            when others => nextstate <= idle;
                
        end case;
        
    end process;

end Behavioral;
