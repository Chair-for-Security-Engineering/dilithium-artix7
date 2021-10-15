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

entity macc_poly is
    Port (
        clk : in std_logic;
        d   : in macc_poly_in_type;
        q   : out macc_poly_out_type
    );
end macc_poly;

architecture Behavioral of macc_poly is
    
    constant MACC_DELAY : natural := 14;
    constant ADD_DELAY : natural := 4;
    constant MEMORY_READ_DELAY : natural := GLOBAL_MEMORY_DELAY;
    constant PIPELINE_DELAY_MAX : natural := MEMORY_READ_DELAY + MACC_DELAY;
    
    type state_type is (idle, multiply, multiply_finish, multiply_shortcut);
    signal state, nextstate : state_type;
    
    signal maccd, maccd_reg : macc_coeff_in_type;
    signal maccq : macc_coeff_out_type;

    type memcnt_type is array(0 to 1) of std_logic_vector(5 downto 0);
    signal memcnt : memcnt_type;
    signal memcnt_en, memcnt_rst, memcnt_ovf : std_logic_vector(0 to 1);
    
    type pipeline_type is record
        memcnt_en : std_logic;
        rwen : std_logic;
        maccd_en : std_logic;
    end record pipeline_type;
    signal pipeline_in, pipeline_out : pipeline_type;
    signal pipeline_en : std_logic;
    type pipeline_array is array(1 to PIPELINE_DELAY_MAX) of pipeline_type;
    signal pipeline : pipeline_array := (others => (memcnt_en => '0', maccd_en => '0', rwen => '0'));
    signal pipeline_delay : natural range MEMORY_READ_DELAY+ADD_DELAY to MEMORY_READ_DELAY+MACC_DELAY;
    
    signal rwen : std_logic;

begin

process(clk)
begin
    if rising_edge(clk)
    then
        maccd_reg <= maccd;
    end if;
end process;

macc: entity work.macc_coeff
port map (
    clk => clk,
    d => maccd_reg,
    q => maccq
);
maccd.a <= d.memq_a;
maccd.b <= d.memq_b;
maccd.c <= d.memq_c;
maccd.op <= d.op;

assert PIPELINE_DELAY_MAX >= 1 report "bad pipeline delay" severity failure;
pipeline_delay <= MEMORY_READ_DELAY+ADD_DELAY when d.op /= op_macc else MEMORY_READ_DELAY+MACC_DELAY; 
maccd.en <= pipeline(MEMORY_READ_DELAY).maccd_en -- after MEMORY_READ_DELAY_CYCLES
            or pipeline(PIPELINE_DELAY).maccd_en; -- at the end of the pipeline
pipeline_out <= pipeline(PIPELINE_DELAY);
memcnt_en(1) <= pipeline_out.memcnt_en;
pip: process(clk)
begin
    if rising_edge(clk)
    then
        if pipeline_en = '1'
        then
            for i in PIPELINE_DELAY_MAX downto 2
            loop
                pipeline(i) <= pipeline(i-1);
            end loop;
            pipeline(1) <= pipeline_in;
        else
            pipeline <= (others => (others => '0'));
        end if;
    end if;
end process;
pipeline_in.memcnt_en <= memcnt_en(0);
pipeline_in.rwen <= rwen;

memory_counter: process(clk)
begin
    if rising_edge(clk)
    then
        for i in 0 to 1
        loop
            if memcnt_rst(i) = '1'
            then
                memcnt(i) <= (others => '0');
                memcnt_ovf(i) <= '0';
            elsif memcnt_en(i) = '1' and memcnt_ovf(i) = '0'
            then
                memcnt(i) <= std_logic_vector(unsigned(memcnt(i)) + 1);
                if memcnt(i) = "111111"
                then
                    memcnt(i) <= "000000";
                    memcnt_ovf(i) <= '1';
                end if;
            end if;
        end loop;
    end if;
end process;


-- output signals
addrgen: for i in 0 to 3
generate
    q.raddr(i)(7 downto 2) <= memcnt(0);
    q.raddr(i)(1 downto 0) <= std_logic_vector(to_unsigned(i, 2));
    
    q.waddr(i)(7 downto 2) <= memcnt(1);
    q.waddr(i)(1 downto 0) <= std_logic_vector(to_unsigned(i, 2));
end generate;

q.wdata(0) <= maccq(0);
q.wdata(1) <= maccq(1);
q.wdata(2) <= maccq(2);
q.wdata(3) <= maccq(3);
q.wen <= pipeline_out.rwen;
q.ren <= rwen;





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

values: process(state, d.en, memcnt_ovf)
begin
    nextstate <= state;
    
    case state is
        
        when idle =>
            -- internal signals
            memcnt_rst <= "11";
            memcnt_en(0) <= '0';
            pipeline_in.maccd_en <= '0';
            pipeline_en <= '0';
            rwen <= '0';
            
            -- output signals
            q.ready <= '1';
            q.ready_read <= '1';
            
            if d.en = '1'
            then
                nextstate <= multiply;
            end if;
        
        when multiply =>
            -- internal signals
            memcnt_rst <= "00";
            memcnt_en(0) <= '1';
            pipeline_in.maccd_en <= '1';
            pipeline_en <= '1';
            rwen <= '1';
            
            -- output signals
            q.ready <= memcnt_ovf(1);
            q.ready_read <= '0';
            
            if memcnt_ovf(0) = '1'
            then
                rwen <= '0';
                nextstate <= multiply_finish;
            end if;
        
        when multiply_finish =>
            -- internal signals
            memcnt_rst <= "00";
            memcnt_en(0) <= '0';
            pipeline_in.maccd_en <= '0';
            pipeline_en <= '1';
            rwen <= '0';
            
            -- output signals
            q.ready <= '0';
            q.ready_read <= '1';
            
            if memcnt_ovf(1) = '1' -- done
            then
                nextstate <= idle;
            end if;
            if d.en = '1' -- short cut to the next polynomial
            then
                memcnt_rst(0) <= '1';
                nextstate <= multiply_shortcut;
            end if;
        
        when multiply_shortcut =>
            -- internal signals
            memcnt_rst <= "00";
            memcnt_en(0) <= '1';
            pipeline_in.maccd_en <= '1';
            pipeline_en <= '1';
            rwen <= '1';
            
            -- output signals
            q.ready <= '0';
            q.ready_read <= '0';
            
            if memcnt_ovf(1) = '1'
            then
                q.ready <= '1'; -- indicate we're starting to write the next polynomial
                memcnt_rst(1) <= '1';
                nextstate <= multiply;
            end if;
        
    end case;
    
end process;

end Behavioral;
