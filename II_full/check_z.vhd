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

entity check_z is
    Port (
        clk : in std_logic;
        d   : in check_z_in_type;
        q   : out check_z_out_type
    );
end check_z;

architecture Behavioral of check_z is

type state_type is (idle, check, check_enabled, check_finish, finished_good, finished_bad);
signal state, nextstate : state_type;

signal memcntd : counter_in_type;
signal memcntq : counter_out_type;
signal memcnt : natural range 0 to DILITHIUM_k*DILITHIUM_N/4-1;
signal memcntd_en_delayed, reg_en : std_logic;

type pipeline_type is array(1 to GLOBAL_MEMORY_DELAY) of natural range 0 to NUM_MEM_8_POLY-1;
signal pipeline : pipeline_type;

signal valid : std_logic_vector(0 to 3);
signal valid_ct0 : std_logic_vector(0 to 3);

begin

pip: process(clk)
begin
    if rising_edge(clk)
    then
        for i in GLOBAL_MEMORY_DELAY downto 2
        loop
            pipeline(i) <= pipeline(i-1);
        end loop;
        if d.ct0 = '0'
        then
            if memcnt / (DILITHIUM_N/4) < DILITHIUM_l
            then
                pipeline(1) <= memory_map.zy(memcnt / (DILITHIUM_N/4)).memory_index;
            else
                pipeline(1) <= 0;
            end if;
        else
            if memcnt / (DILITHIUM_N/4) < DILITHIUM_k
            then
                pipeline(1) <= memory_map.tmp1(memcnt / (DILITHIUM_N/4)).memory_index;
            else
                pipeline(1) <= 0;
            end if;
        end if;
    end if;
end process;

vgen: for i in 0 to 3
generate
    valid(i) <= '1' when 
    to_integer(unsigned(d.memq(pipeline(GLOBAL_MEMORY_DELAY))(i))) < (2**DILITHIUM_loggamma1) - DILITHIUM_beta 
    or to_integer(unsigned(d.memq(pipeline(GLOBAL_MEMORY_DELAY))(i))) > DILITHIUM_Q - (2**DILITHIUM_loggamma1) + DILITHIUM_beta
    else '0';
end generate;

vct0gen: for i in 0 to 3
generate
    valid_ct0(i) <= '1' when 
    to_integer(unsigned(d.memq(pipeline(GLOBAL_MEMORY_DELAY))(i))) < DILITHIUM_gamma2 
    or to_integer(unsigned(d.memq(pipeline(GLOBAL_MEMORY_DELAY))(i))) > DILITHIUM_Q - DILITHIUM_gamma2
    else '0';
end generate;

md: entity work.dyn_shift_reg
generic map (width => 1, max_depth => GLOBAL_MEMORY_DELAY)
port map (
    clk => clk,
    ce => reg_en,
    depth => GLOBAL_MEMORY_DELAY,
    d(0) => memcntd.en,
    q(0) => memcntd_en_delayed
);

memm: process(state, memcnt, memcntd)
variable m,p : natural;
begin
    q.memd <= (others => ZEROMEM);
    case state is
        when check | check_enabled | check_finish =>
            if d.ct0 = '1'
            then
                m := memory_map.tmp1(memcnt / (DILITHIUM_N/4)).memory_index;
                p := memory_map.tmp1(memcnt / (DILITHIUM_N/4)).poly_index;
            else
                m := memory_map.zy(memcnt / (DILITHIUM_N/4)).memory_index;
                p := memory_map.zy(memcnt / (DILITHIUM_N/4)).poly_index;
            end if;
            q.memd(m).rsel <= p;
            q.memd(m).ren <= (others => memcntd.en);
            for i in 0 to 3
            loop
                q.memd(m).raddr(i) <= std_logic_vector(to_unsigned(memcnt mod (DILITHIUM_N/4), 6)) & std_logic_vector(to_unsigned(i, 2));
            end loop;
        when others =>
    end case;
end process;

memory_counter: entity work.counter
generic map (max_value => DILITHIUM_k*DILITHIUM_N/4-1)
port map (
    clk => clk,
    d => memcntd,
    q => memcntq,
    value => memcnt
);

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

signals: process(state, d, memcntq, memcnt, memcntd_en_delayed, valid, valid_ct0)
begin

    nextstate <= state;
    
    q.ready <= '0';
    q.valid <= '0';
    q.result_bad <= '0';
    
    memcntd.en <= '0';
    memcntd.rst <= '0';
    
    reg_en <= '1';
    
    case state is
        when idle =>
            q.ready <= '1';
            memcntd.rst <= '1';
            reg_en <= '1'; -- strange errors if we don't leave it on
            if d.en = '1'
            then
                nextstate <= check;
            end if;
            
        when check =>
            memcntd.en <= '1';
            if memcntd_en_delayed = '1'
            then
                nextstate <= check_enabled;
            end if;
        
        when check_enabled =>
            memcntd.en <= '1';
            if (valid /= "1111" and d.ct0 = '0') or (valid_ct0 /= "1111" and d.ct0 = '1') 
            then
                nextstate <= finished_bad;
            elsif (memcntq.max = '1' and d.ct0 = '1') or (memcnt = DILITHIUM_l*DILITHIUM_N/4-2 and d.ct0 = '0')
            then
                nextstate <= check_finish;
            end if;
            
        when check_finish =>
            if memcntd_en_delayed = '0'
            then
                nextstate <= finished_good;
            elsif (valid /= "1111" and d.ct0 = '0') or (valid_ct0 /= "1111" and d.ct0 = '1') 
            then
                nextstate <= finished_bad;
            end if;
            
        when finished_good =>
            q.ready <= '1';
            q.valid <= '1';
            q.result_bad <= '0';
            
        when finished_bad =>
            q.ready <= '1';
            q.valid <= '1';
            q.result_bad <= '1';
            
    end case;

end process;

end Behavioral;
