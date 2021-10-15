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

entity matmul is
    Port (
        clk : in std_logic;
        d   : in matmul_in_type;
        q   : out matmul_out_type
    );
end matmul;

architecture Behavioral of matmul is

    type state_type is (idle, ntt_first, ntt_wait_first, ntt_start, 
    macc_start, macc, macc_shortcut, macc_finish, ntt_finish);
    signal state, nextstate : state_type;
    
    
    -- counter
    type lcnt_type is array(0 to 1) of natural range 0 to DILITHIUM_l-1;
    signal lcnt : lcnt_type;
    signal l_en, l_rst, l_ovf : std_logic_vector(0 to 1);
    signal l_macc : natural range 0 to DILITHIUM_l-1;
    signal l_macc_en, l_macc_rst, l_macc_max : std_logic;
    type kcnt_type is array(0 to 2) of natural range 0 to DILITHIUM_k-1;
    signal kcnt : kcnt_type;
    signal k_en, k_rst, k_max : std_logic_vector(0 to 1);
    
    -- memory
    constant MEMORY_DELAY : natural := GLOBAL_MEMORY_DELAY;
    signal mem_ntt, mem_macc, mem_macc_read_resvec : std_logic;
    type memm_type is array(0 to 3) of natural range 0 to NUM_MEM_8_POLY-1;
    type pipeline_type is array(1 to MEMORY_DELAY) of memm_type;
    signal pipeline_in, pipeline_out : memm_type;
    signal pipeline : pipeline_type;
    
    -- vecaddr reg
    signal addr_reg_en : std_logic := '0';
    signal vecaddr, resvecaddr : matvecaddr_type := none;
    
begin

-- vecaddr reg
process(clk)
begin
    if rising_edge(clk)
    then
        if addr_reg_en = '1'
        then
            vecaddr <= d.vecaddr;
            resvecaddr <= d.resvecaddr;
        end if;
    end if;
end process;

-- counter
cntr: process(clk)
begin
    if rising_edge(clk)
    then
        if l_macc_rst = '1'
        then
            l_macc <= 0;
            l_macc_max <= '0';
        elsif l_macc_en = '1' and l_macc_max = '0'
        then
            l_macc <= l_macc + 1;
            if l_macc = DILITHIUM_l-2
            then
                l_macc_max <= '1';
            end if;
        end if;
    
        for i in 0 to 1
        loop
            if l_rst(i) = '1'
            then
                lcnt(i) <= 0;
                l_ovf(i) <= '0';
            elsif l_en(i) = '1' and l_ovf(i) = '0'
            then
                lcnt(i) <= lcnt(i) + 1;
                if lcnt(i) = DILITHIUM_l-1
                then
                    lcnt(i) <= DILITHIUM_l-1;
                    l_ovf(i) <= '1';
                end if;
            end if;
        
            if k_rst(i) = '1'
            then
                kcnt(i) <= 0;
                k_max(i) <= '0';
            elsif k_en(i) = '1' and k_max(i) = '0'
            then
                kcnt(i) <= kcnt(i) + 1;
                if kcnt(i) = DILITHIUM_k-2
                then
                    kcnt(i) <= DILITHIUM_k-1;
                    k_max(i) <= '1';
                end if;
            end if;
        end loop;
    end if;
end process;

-- state machine
states: process(clk)
begin
    if rising_edge(clk)
    then
        state <= nextstate;
    end if;
end process;

values: process(state, d.en, d.nttq.ready, d.nttq.ready_read, d.maccq.ready, d.maccq.ready_read, k_max, l_ovf, l_macc, l_macc_max)
begin
    nextstate <= state;
    
    -- standard values:
    q.ready <= '1';
    
    -- memory
    mem_ntt <= '0';
    mem_macc <= '0';
    mem_macc_read_resvec <= '0';
    
    -- counter ctrl
    l_en <= "00";
    l_rst <= "11";
    l_macc_en <= '0';
    l_macc_rst <= '1';
    k_en <= "00";
    k_rst <= "11";
    
    -- ntt
    q.nttd.en <= '0';
    q.nttd.inv <= '0';
    
    -- macc
    q.maccd.en <= '0';
    q.maccd.rst <= '0';
    
    
    addr_reg_en <= '0';

    case state is
        when idle =>
            q.maccd.rst <= '1';
            if d.en = '1'
            then
                addr_reg_en <= '1';
                nextstate <= ntt_first;
            end if;
            
        when ntt_first =>
            q.ready <= '0';
    
            -- counter
            l_en <= "00";
            l_rst <= "00";
            k_en <= "00";
            k_rst <= "00";
            
            -- ntt
            q.nttd.en <= '1';
            q.nttd.inv <= '0';
            mem_ntt <= '1';
            
            if d.nttq.ready = '0' -- started
            then
                nextstate <= ntt_wait_first;
            end if;
            
        when ntt_wait_first =>
            q.ready <= '0';
    
            -- counter
            l_en <= "00";
            l_rst <= "00";
            k_en <= "00";
            k_rst <= "00";
            
            -- ntt
            q.nttd.en <= '0';
            q.nttd.inv <= '0';
            mem_ntt <= '1';
            
            if d.nttq.ready_read = '1' -- finished reading for first ntt
            then
                l_en(0) <= '1'; -- transform next polynomial
                nextstate <= ntt_start;
            end if;
            
        when ntt_start =>
            q.ready <= '0';
            
            -- counter
            l_en <= "00";
            l_rst <= "00";
            k_en <= "00";
            k_rst <= "00";
            l_macc_en <= '0';
            l_macc_rst <= '0';
            
            -- ntt
            q.nttd.en <= not l_ovf(0);
            q.nttd.inv <= '0';
            mem_ntt <= '1';
            
            if d.nttq.ready = '1' -- finished transforming last iteration
            then
                if l_ovf(0) = '1'
                then
                    l_macc_en <= '1';
                    nextstate <= macc_start;
                else
                    l_en(1) <= '1'; -- write next polynomial
                    nextstate <= macc_start;
                end if;
            end if;
            
            
        when macc_start =>
            q.ready <= '0';
            
            -- counter
            l_en <= "00";
            l_rst <= "00";
            k_en <= "00";
            k_rst <= "00";
            l_macc_en <= '0';
            l_macc_rst <= '0';
            
            -- ntt / memory
            if l_macc_max = '0'
            then
                mem_ntt <= '1';
                q.nttd.en <= '0';
                q.nttd.inv <= '0';
            end if; -- else: standard (do nothing, reset)
            
            -- macc / memory
            q.maccd.en <= '1';
            mem_macc <= '1';
            if l_macc > 0
            then
                mem_macc_read_resvec <= '1';
            end if;
            
            if d.maccq.ready = '0' -- started
            then
                nextstate <= macc;
            end if;
            
            
        when macc =>
            q.ready <= '0';
            
            -- counter
            l_en <= "00";
            l_rst <= "00";
            k_en <= "00";
            k_rst <= "00";
            l_macc_en <= '0';
            l_macc_rst <= '0';
            
            -- ntt
            if l_macc_max = '0'
            then
                mem_ntt <= '1';
                q.nttd.en <= '0';
                q.nttd.inv <= '0';
            end if; -- else: standard (do nothing, reset)
            
            -- macc
            q.maccd.en <= '0';
            mem_macc <= '1';
            if l_macc > 0
            then
                mem_macc_read_resvec <= '1';
            end if;
            
            if d.maccq.ready_read = '1' -- finished
            then
                if k_max(0) = '0'
                then
                    k_en(0) <= '1'; -- macc next poly
                    nextstate <= macc_shortcut;
                else
                    nextstate <= macc_finish;
                end if;
            end if;
            
        when macc_shortcut =>
            q.ready <= '0';
            
            -- counter
            l_en <= "00";
            l_rst <= "00";
            k_en <= "00";
            k_rst <= "00";
            l_macc_en <= '0';
            l_macc_rst <= '0';
            
            -- ntt
            if l_macc_max = '0'
            then
                mem_ntt <= '1';
                q.nttd.en <= '0';
                q.nttd.inv <= '0';
            end if; -- else: standard (do nothing, reset)
            
            -- macc
            q.maccd.en <= '1';
            mem_macc <= '1';
            if l_macc > 0
            then
                mem_macc_read_resvec <= '1';
            end if;
            
            if d.maccq.ready_read = '0' -- started
            then
                nextstate <= macc_finish;
            end if;
            
        when macc_finish =>
            q.ready <= '0';
            
            -- counter
            l_en <= "00";
            l_rst <= "00";
            k_en <= "00";
            k_rst <= "00";
            l_macc_en <= '0';
            l_macc_rst <= '0';
            
            -- ntt
            if l_macc_max = '0'
            then
                mem_ntt <= '1';
                q.nttd.en <= '0';
                q.nttd.inv <= '0';
            end if; -- else: standard (do nothing, reset)
            
            -- macc
            q.maccd.en <= '0';
            mem_macc <= '1';
            if l_macc > 0
            then
                mem_macc_read_resvec <= '1';
            end if;
            
            if d.maccq.ready = '1' -- finished
            then
                if k_max(1) = '1'
                then
                    mem_macc <= '0';
                    if l_macc_max = '1'
                    then
                        nextstate <= idle; -- DONE
                    else
                        l_macc_en <= '1'; -- increment for next round
                        nextstate <= ntt_finish;
                    end if;
                else
                    k_en(1) <= '1'; -- write next poly
                    nextstate <= macc;
                end if;
            end if;
            
            
        when ntt_finish =>
            q.ready <= '0';
            
            -- counter
            l_en <= "00";
            l_rst <= "00";
            k_en <= "00";
            k_rst <= "11";
            l_macc_en <= '0';
            l_macc_rst <= '0';
            
            -- ntt
            q.nttd.en <= '0';
            q.nttd.inv <= '0';
            mem_ntt <= '1';
            
            -- macc
            q.maccd.en <= '0';
            
            if d.nttq.ready_read = '1' -- finished reading
            then
                l_en(0) <= '1'; -- transform next poly
                nextstate <= ntt_start;
            end if;
        
        when others => nextstate <= idle;
            
    end case;
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

memfwdmux: process(d.memq, pipeline_out, mem_ntt, mem_macc, mem_macc_read_resvec) 
begin
    q.nttd.data <= (others => (others => '0'));
    q.maccd.memq_a <= (others => (others => '0'));
    q.maccd.memq_b <= (others => (others => '0'));
    q.maccd.memq_c <= (others => (others => '0'));
    
    for m in 0 to NUM_MEM_8_POLY-1
    loop
        if pipeline_out(0) = m and mem_ntt = '1'
        then
            q.nttd.data <= d.memq(m);
        end if;
        if pipeline_out(1) = m and mem_macc = '1'
        then
            q.maccd.memq_a <= d.memq(m);
        end if;
        if pipeline_out(2) = m and mem_macc = '1'
        then
            q.maccd.memq_b <= d.memq(m);
        end if;
        if pipeline_out(3) = m and mem_macc_read_resvec = '1'
        then
            q.maccd.memq_c <= d.memq(m);
        end if;
    end loop;
end process;

meminmux: process(kcnt, lcnt, l_macc, vecaddr, resvecaddr, d.maccq, d.nttq, mem_ntt, mem_macc, mem_macc_read_resvec)
variable mnttr, mnttw, mA, mmaccr, mresr, mresw : natural;
variable pnttr, pnttw, pA, pmaccr, presr, presw : natural;
variable flag : boolean;
begin
    -- initialize
    q.memd <= (others => ZEROMEM);
    
    flag := false;
    
    mnttr := 0;
    mnttw := 0;
    mmaccr := 0;
    mresr := 0;
    mresw := 0;
    pnttr := 0;
    pnttw := 0;
    pmaccr := 0;
    presr := 0;
    presw := 0;
    
    mA := memory_map.A(kcnt(0), l_macc).memory_index;
    pA := memory_map.A(kcnt(0), l_macc).poly_index;
    case vecaddr is
        when t1 =>
            mnttr := memory_map.t1(lcnt(0)).memory_index;
            mnttw := memory_map.t1(lcnt(1)).memory_index;
            pnttr := memory_map.t1(lcnt(0)).poly_index;
            pnttw := memory_map.t1(lcnt(1)).poly_index;
            mmaccr := memory_map.t1(l_macc).memory_index;
            pmaccr := memory_map.t1(l_macc).poly_index;
        when s1 =>
            mnttr := memory_map.s1(lcnt(0)).memory_index;
            mnttw := memory_map.s1(lcnt(1)).memory_index;
            pnttr := memory_map.s1(lcnt(0)).poly_index;
            pnttw := memory_map.s1(lcnt(1)).poly_index;
            mmaccr := memory_map.s1(l_macc).memory_index;
            pmaccr := memory_map.s1(l_macc).poly_index;
        when z =>
            mnttr := memory_map.zy(lcnt(0)).memory_index;
            mnttw := memory_map.zy(lcnt(1)).memory_index;
            pnttr := memory_map.zy(lcnt(0)).poly_index;
            pnttw := memory_map.zy(lcnt(1)).poly_index;
            mmaccr := memory_map.zy(l_macc).memory_index;
            pmaccr := memory_map.zy(l_macc).poly_index;
        when y =>
            mnttr := memory_map.zy(lcnt(0)).memory_index;
            mnttw := memory_map.zy(lcnt(1)).memory_index;
            pnttr := memory_map.zy(lcnt(0)).poly_index;
            pnttw := memory_map.zy(lcnt(1)).poly_index;
            mmaccr := memory_map.zy(l_macc).memory_index;
            pmaccr := memory_map.zy(l_macc).poly_index;
        when others => flag := true;
    end case;
    case resvecaddr is
        when t0 =>
            mresr := memory_map.t0(kcnt(0)).memory_index;
            presr := memory_map.t0(kcnt(0)).poly_index;
            mresw := memory_map.t0(kcnt(1)).memory_index;
            presw := memory_map.t0(kcnt(1)).poly_index;
        when w =>
            mresr := memory_map.w(kcnt(0)).memory_index;
            presr := memory_map.w(kcnt(0)).poly_index;
            mresw := memory_map.w(kcnt(1)).memory_index;
            presw := memory_map.w(kcnt(1)).poly_index;
        when others => flag := true;
    end case;
    
    pipeline_in(0) <= mnttr;
    pipeline_in(1) <= mA;
    pipeline_in(2) <= mmaccr;
    pipeline_in(3) <= mresr;

    if flag = false
    then
        -- ntt
        if mem_ntt = '1'
        then
            q.memd(mnttr).raddr <= d.nttq.raddr;
            q.memd(mnttr).rsel <= pnttr;
            q.memd(mnttr).ren <= (others => d.nttq.ren);
            
            q.memd(mnttw).waddr <= d.nttq.waddr;
            q.memd(mnttw).wsel <= pnttw;
            q.memd(mnttw).wen <= (others => d.nttq.wen);
            q.memd(mnttw).wdata <= d.nttq.wdata;
        end if;
        
        -- macc
        if mem_macc = '1'
        then
            q.memd(mA).raddr <= d.maccq.raddr;
            q.memd(mA).rsel <= pA;
            q.memd(mA).ren <= (others => d.maccq.ren);
            
            q.memd(mmaccr).raddr <= d.maccq.raddr;
            q.memd(mmaccr).rsel <= pmaccr;
            q.memd(mmaccr).ren <= (others => d.maccq.ren);
            
            q.memd(mresw).waddr <= d.maccq.waddr;
            q.memd(mresw).wsel <= presw;
            q.memd(mresw).wen <= (others => d.maccq.wen);
            q.memd(mresw).wdata <= d.maccq.wdata;
        end if;
        
        if mem_macc_read_resvec = '1'
        then
            q.memd(mresr).raddr <= d.maccq.raddr;
            q.memd(mresr).rsel <= presr;
            q.memd(mresr).ren <= (others => d.maccq.ren);
        end if;
    end if;
end process;

end Behavioral;
