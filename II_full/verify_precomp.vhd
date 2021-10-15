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

entity verify_precomp is
    Port (
        clk : in std_logic;
        d   : in verify_precomp_in_type;
        q   : out verify_precomp_out_type
    );
end verify_precomp;

architecture Behavioral of verify_precomp is
    
    type state_type is (idle, crt1, crt1_finish, expandA, expandA_finish, nttt1, nttt1_finish, nttt1_shortcut);
    signal state, nextstate : state_type;
    
    signal kcntd : counter_in_type;
    signal kcntq : counter_out_type;
    signal kcnt : natural range 0 to DILITHIUM_k-1;
    
    type pip_t is array(1 to GLOBAL_MEMORY_DELAY) of natural range 0 to NUM_MEM_8_POLY-1;
    signal pip : pip_t;
    signal pip_in : natural range 0 to NUM_MEM_8_POLY-1;
    
begin

kcounter: entity work.counter
generic map (max_value => DILITHIUM_k-1)
port map (
    clk => clk,
    d => kcntd,
    q => kcntq,
    value => kcnt
);

q.crt1d.memq <= d.memq;
q.crt1d.keccakq <= d.keccakq;
q.crt1d.rhoregq <= d.rhoregq;

q.expandAd.seedregq <= d.rhoregq;
q.expandAd.keccakq <= d.keccakq;

q.trregd <= d.crt1q.trregd;

pipeline: process(clk)
begin
    if rising_edge(clk)
    then
        for i in GLOBAL_MEMORY_DELAY downto 2
        loop
            pip(i) <= pip(i-1);
        end loop;
        pip(1) <= pip_in;
    end if;
end process;

mux: process(state, d, kcntq, kcnt, pip)
variable m,p : natural;
begin
    m := 0;
    p := 0;
    q.memd <= (others => ZEROMEM);
    q.keccakd <= ZEROKECCAK;
    q.rhoregd <= ZEROREG32;
    pip_in <= 0;
    q.nttd.data <= (others => (others => '0'));
    case state is
        when crt1 | crt1_finish =>
            q.keccakd <= d.crt1q.keccakd;
            q.memd <= d.crt1q.memd;
            q.rhoregd <= d.crt1q.rhoregd;
            
        when expandA | expandA_finish | nttt1 | nttt1_finish | nttt1_shortcut =>
            q.keccakd <= d.expandAq.keccakd;
            q.memd <= d.expandAq.memd;
            q.rhoregd <= d.expandAq.seedregd;
            
            m := memory_map.t1(kcnt).memory_index;
            p := memory_map.t1(kcnt).poly_index;
            q.memd(m).ren <= (others => d.nttq.ren);
            q.memd(m).raddr <= d.nttq.raddr;
            q.memd(m).rsel <= p;
            pip_in <= m;
            
            if state = nttt1_shortcut and kcnt > 0 and kcntq.ovf = '0'
            then
                m := memory_map.t1(kcnt-1).memory_index;
                p := memory_map.t1(kcnt-1).poly_index;
            end if;
            q.memd(m).wen <= (others => d.nttq.wen);
            q.memd(m).waddr <= d.nttq.waddr;
            q.memd(m).wsel <= p;
            q.memd(m).wdata <= d.nttq.wdata;
            
            q.nttd.data <= d.memq(pip(GLOBAL_MEMORY_DELAY));
            
        when others =>
    end case;
end process;

states: process(clk)
begin
    if rising_edge(clk)
    then
        state <= nextstate;
    end if;
end process;

signals: process(state, d)
begin
    nextstate <= state;
    
    kcntd.en <= '0';
    kcntd.rst <= '0';
    
    q.ready <= '0';
    q.crt1d.en <= '0';
    q.expandAd.en <= '0';
    
    q.nttd.en <= '0';
    q.nttd.inv <= '0';
    
    case state is
        when idle => 
            q.ready <= '1';
            kcntd.rst <= '1';
            if d.en = '1' and d.crt1q.ready = '1'
            then
                nextstate <= crt1;
            end if;
        
        when crt1 =>
            q.crt1d.en <= '1';
            if d.crt1q.ready = '0'
            then
                nextstate <= crt1_finish;
            end if;
            
        when crt1_finish =>
            if d.crt1q.ready = '1' and d.expandAq.ready = '1'
            then
                nextstate <= expandA;
            end if;
            
        when expandA =>
            q.expandAd.en <= '1';
            if d.expandAq.ready = '0'
            then
                nextstate <= nttt1;
            end if;

        when nttt1 =>
            q.nttd.en <= '1';
            
            if d.nttq.ready_read = '0'
            then
                nextstate <= nttt1_finish;
            end if;
                
        when nttt1_finish =>
            q.nttd.en <= '0';
            
            if d.nttq.ready_read = '1'
            then
                kcntd.en <= '1';
                nextstate <= nttt1_shortcut;
            end if;
            
        when nttt1_shortcut =>
            if kcntq.ovf = '0'
            then
                q.nttd.en <= '1';
            end if;
            
            if d.nttq.ready = '1'
            then
                if kcntq.ovf = '0'
                then
                    nextstate <= nttt1_finish;
                else
                    nextstate <= expandA_finish;
                end if;
            end if;
            
        when expandA_finish =>
            if d.expandAq.ready = '1'
            then
                nextstate <= idle;
            end if;
    end case;

end process;

end Behavioral;
