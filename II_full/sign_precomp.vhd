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

library work;
use work.dilithium.all;
use work.interfaces.all;
use work.memmap.all;

entity sign_precomp is
    Port (
        clk : in std_logic;
        d   : in sign_precomp_in_type;
        q   : out sign_precomp_out_type
    );
end sign_precomp;

architecture Behavioral of sign_precomp is

    type state_type is (idle, expandA, expandA_finish, ntts1, ntts1_finish, ntts1_shortcut, ntts2, ntts2_finish, ntts2_shortcut, nttt0, nttt0_finish, nttt0_shortcut);
    signal state, nextstate : state_type;
    
    signal rcntd, wcntd : counter_in_type;
    signal rcntq, wcntq : counter_out_type;
    signal rcnt, wcnt : natural range 0 to DILITHIUM_k-1;
    
    type pip_t is array(1 to GLOBAL_MEMORY_DELAY) of natural range 0 to NUM_MEM_8_POLY-1;
    signal pipeline : pip_t := (others => 0);
    signal pipeline_in : natural range 0 to NUM_MEM_8_POLY-1;
    
begin

    -- counters
    polyvec_read_counter: entity work.counter
    generic map (max_value => DILITHIUM_k-1)
    port map (
        clk => clk,
        d => rcntd,
        q => rcntq,
        value => rcnt
    );

    polyvec_write_counter: entity work.counter
    generic map (max_value => DILITHIUM_k-1)
    port map (
        clk => clk,
        d => wcntd,
        q => wcntq,
        value => wcnt
    );
    
    -- pipeline for memory index
    pip: process(clk)
    begin
        if rising_edge(clk)
        then
            for i in GLOBAL_MEMORY_DELAY downto 2
            loop
                pipeline(i) <= pipeline(i-1);
            end loop;
            pipeline(1) <= pipeline_in;
        end if;
    end process;
    
    -- memory mux
    mm: process(state, d.expandAq.memd, d.nttq, d.memq, rcnt, wcnt, pipeline)
    variable m : natural;
    begin
        
        q.memd <= d.expandAq.memd;
        q.nttd.data <= (others => (others => '0'));
        m := 0;
        
        case state is
            when ntts1 | ntts1_finish | ntts1_shortcut =>
                m := memory_map.s1(wcnt).memory_index;
                q.memd(m).wsel <= memory_map.s1(wcnt).poly_index;
                q.memd(m).wen <= (others => d.nttq.wen);
                q.memd(m).waddr <= d.nttq.waddr;
                q.memd(m).wdata <= d.nttq.wdata;
                m := memory_map.s1(rcnt).memory_index;
                q.memd(m).rsel <= memory_map.s1(rcnt).poly_index;
                q.memd(m).ren <= (others => d.nttq.ren);
                q.memd(m).raddr <= d.nttq.raddr;
                q.nttd.data <= d.memq(pipeline(GLOBAL_MEMORY_DELAY));
                
            when ntts2 | ntts2_finish | ntts2_shortcut =>
                m := memory_map.s2(wcnt).memory_index;
                q.memd(m).wsel <= memory_map.s2(wcnt).poly_index;
                q.memd(m).wen <= (others => d.nttq.wen);
                q.memd(m).waddr <= d.nttq.waddr;
                q.memd(m).wdata <= d.nttq.wdata;
                m := memory_map.s2(rcnt).memory_index;
                q.memd(m).rsel <= memory_map.s2(rcnt).poly_index;
                q.memd(m).ren <= (others => d.nttq.ren);
                q.memd(m).raddr <= d.nttq.raddr;
                q.nttd.data <= d.memq(pipeline(GLOBAL_MEMORY_DELAY));
                
            when nttt0 | nttt0_finish | nttt0_shortcut =>
                m := memory_map.t0(wcnt).memory_index;
                q.memd(m).wsel <= memory_map.t0(wcnt).poly_index;
                q.memd(m).wen <= (others => d.nttq.wen);
                q.memd(m).waddr <= d.nttq.waddr;
                q.memd(m).wdata <= d.nttq.wdata;
                m := memory_map.t0(rcnt).memory_index;
                q.memd(m).rsel <= memory_map.t0(rcnt).poly_index;
                q.memd(m).ren <= (others => d.nttq.ren);
                q.memd(m).raddr <= d.nttq.raddr;
                q.nttd.data <= d.memq(pipeline(GLOBAL_MEMORY_DELAY));
                
            when others =>
        end case;
        
        pipeline_in <= m;
    end process;
    
    -- connect keccak and expandAq
    q.keccakd <= d.expandAq.keccakd;
    q.expandAd.keccakq <= d.keccakq;
    
    -- connect rhoreg and expandAq
    q.rhoregd <= d.expandAq.seedregd;
    q.expandAd.seedregq <= d.rhoregq;
    
    -- only fwd ntt
    q.nttd.inv <= '0';

    states: process(clk)
    begin
        if rising_edge(clk)
        then
            state <= nextstate;
        end if;
    end process;
    
    signals: process(state, d, rcntq, rcnt, wcntq, wcnt)
    begin
        
        nextstate <= state;
        
        -- output signal is only overwritten in state idle
        q.ready <= '0';
        
        -- expand A
        q.expandAd.en <= '0';
        
        -- ntt
        q.nttd.en <= '0';
        rcntd.en <= '0';
        rcntd.rst <= '1';
        wcntd.en <= '0';
        wcntd.rst <= '1';
        
        case state is
            when idle =>
                q.ready <= '1';
            
                if d.en = '1'
                then
                    nextstate <= expandA;
                end if;
            
            -----------------------------------------------------------------
            -- expand A
            -----------------------------------------------------------------
            when expandA =>
                q.expandAd.en <= '1';
                
                if d.expandAq.ready = '0'
                then
                    nextstate <= ntts1;
                end if;
            
            -----------------------------------------------------------------
            -- ntt s1
            -----------------------------------------------------------------
            when ntts1 =>
                rcntd.rst <= '0';
                wcntd.rst <= '0';
                q.nttd.en <= '1';
                
                if d.nttq.ready = '0' -- started
                then
                    nextstate <= ntts1_finish;
                end if;
                
            when ntts1_finish =>
                rcntd.rst <= '0';
                wcntd.rst <= '0';
                q.nttd.en <= '0';
                
                if d.nttq.ready_read = '1' -- finished
                then
                    if rcnt = DILITHIUM_l-1
                    then
                        if d.nttq.ready = '1'
                        then
                            rcntd.rst <= '1';
                            wcntd.rst <= '1';
                            nextstate <= ntts2;
                        end if;
                    else
                        rcntd.en <= '1';
                        nextstate <= ntts1_shortcut;
                    end if;
                end if;
                
            when ntts1_shortcut =>
                rcntd.rst <= '0';
                wcntd.rst <= '0';
                q.nttd.en <= '1';
                
                if d.nttq.ready = '1' -- finished
                then
                    wcntd.en <= '1';
                    nextstate <= ntts1;
                end if;
            
            -----------------------------------------------------------------
            -- ntt s2
            -----------------------------------------------------------------
            when ntts2 =>
                rcntd.rst <= '0';
                wcntd.rst <= '0';
                q.nttd.en <= '1';
                
                if d.nttq.ready = '0' -- started
                then
                    nextstate <= ntts2_finish;
                end if;
                
            when ntts2_finish =>
                rcntd.rst <= '0';
                wcntd.rst <= '0';
                q.nttd.en <= '0';
                
                if d.nttq.ready_read = '1' -- finished
                then
                    if rcnt = DILITHIUM_k-1
                    then
                        if d.nttq.ready = '1'
                        then
                            rcntd.rst <= '1';
                            wcntd.rst <= '1';
                            nextstate <= nttt0;
                        end if;
                    else
                        rcntd.en <= '1';
                        nextstate <= ntts2_shortcut;
                    end if;
                end if;
                
            when ntts2_shortcut =>
                rcntd.rst <= '0';
                wcntd.rst <= '0';
                q.nttd.en <= '1';
                
                if d.nttq.ready = '1' -- finished
                then
                    wcntd.en <= '1';
                    nextstate <= ntts2;
                end if;
            
            -----------------------------------------------------------------
            -- ntt t0
            -----------------------------------------------------------------
            when nttt0 =>
                rcntd.rst <= '0';
                wcntd.rst <= '0';
                q.nttd.en <= '1';
                
                if d.nttq.ready = '0' -- started
                then
                    nextstate <= nttt0_finish;
                end if;
                
            when nttt0_finish =>
                rcntd.rst <= '0';
                wcntd.rst <= '0';
                q.nttd.en <= '0';
                
                if d.nttq.ready_read = '1' -- finished
                then
                    if rcnt = DILITHIUM_k-1
                    then
                        if d.nttq.ready = '1'
                        then
                            rcntd.rst <= '1';
                            wcntd.rst <= '1';
                            nextstate <= expandA_finish; -- DONE
                        end if;
                    else
                        rcntd.en <= '1';
                        nextstate <= nttt0_shortcut;
                    end if;
                end if;
                
            when nttt0_shortcut =>
                rcntd.rst <= '0';
                wcntd.rst <= '0';
                q.nttd.en <= '1';
                
                if d.nttq.ready = '1' -- finished
                then
                    wcntd.en <= '1';
                    nextstate <= nttt0;
                end if;
                
            -----------------------------------------------------------------
            -- wait for expandA to finish
            -----------------------------------------------------------------
            when expandA_finish =>
                q.expandAd.en <= '0';
                
                if d.expandAq.ready = '1'
                then
                    nextstate <= idle; -- IDLE
                end if;
            
        end case;
        
    end process;
    
end Behavioral;
