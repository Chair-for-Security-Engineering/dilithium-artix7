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


entity butterfly_dsp is
    Port ( 
           clk :  in std_logic;
           d   :  in butterfly_in_type;
           q   : out butterfly_out_type
           );
end butterfly_dsp;

architecture Behavioral of butterfly_dsp is

constant BUTTERFLY_CASCADE_DELAY : integer := 1;
constant BUTTERFLY_STAGE_2_DELAY : integer := 4;
constant BUTTERFLY_REDUCER_DELAY : integer := 1;
constant RED_DELAY : integer := 9;

signal d1 : bfu_stage_1_in_type;
signal q1 : bfu_stage_1_out_type;
signal d2 : bfu_stage_2_in_type;
signal q2 : bfu_stage_2_out_type;
signal da : bfu_adder_in_type;
signal qa : bfu_adder_out_type;
signal dr : bfu_reducer_in_type;
signal qr : bfu_reducer_out_type;

signal q2_p_delayed: std_logic_vector(16 downto 0);
signal qa_p_delayed : std_logic_vector(22 downto 0);
signal adder_pos  : std_logic_vector(22 downto 0);

signal redd : red_dsp_in_type;
signal redq : red_dsp_out_type;

signal sign_delayed : std_logic;

signal delay_omega_out : std_logic_vector(5 downto 0);
signal delay_a_out, delay_delay_a_out, delay_b_out : std_logic_vector(22 downto 0);
signal delay_bfs1p_out : std_logic_vector(16 downto 0);

signal bfad_inmux_out : std_logic_vector(24 downto 0);
signal bfad_inmux_ctrl : std_logic_vector(1 downto 0);

signal bfap_pos, bfap_delay_out, delay_output_a_in, redq_data_delayed, adder_pos_delayed : std_logic_vector(22 downto 0);
--signal final_delay : integer range 0 to 63;

begin

-- STAGE 1
stage1: entity work.bfu_stage_1
port map (
    clk => clk,
    d => d1,
    q => q1
);

-- stage 1 inputs
d1.a <= "0" & d.b when d.inv = '0' else d.a_minus_b;
d1.b <= d.omega(16 downto 0);
d1.c <= d.a;
d1.sel <= d.inv;
d1.en <= d.en;







-- STAGE 2 starts at the same cycle!
stage2: entity work.bfu_stage_2
port map (
    clk => clk,
    d => d2,
    q => q2
);

-- stage 2 inputs
d2.acin <= q1.acout;
d2.pcin <= q1.pcout;
d2.en <= d.en;
delay_sign: process(clk)
begin
    if rising_edge(clk)
    then
        if d.inv = '1' and d.en = '1'
        then
            sign_delayed <= d.a_minus_b(23);
        end if;
    end if;
end process;
d2.sign <= sign_delayed when d.inv = '1' else '0';

delay_omega_high: entity work.dyn_shift_reg
generic map (width => 6, max_depth => BUTTERFLY_CASCADE_DELAY)
port map (
    clk => clk,
    ce => d.en,
    depth => BUTTERFLY_CASCADE_DELAY,
    d => d.omega(22 downto 17),
    q => d2.b
);







-- ADDER
adder: entity work.bfu_adder
port map (
    clk => clk,
    d => da,
    q => qa
);

-- adder inputs
da.a <= "0" & d.a(22 downto 1) when d.inv = '1' else redq.data;
da.d <= (others => '0') when d.a(0) = '0' and d.b(0) = '0' else
        std_logic_vector(to_unsigned(1, 24)) when d.a(0) = '1' and d.b(0) = '1' else
        std_logic_vector(to_unsigned((DILITHIUM_Q+1)/2, 24)); -- only meaningful for inv NTT
da.sel <= d.inv;
da.en <= d.en;

delay_a: entity work.dyn_shift_reg
generic map (width => 23, max_depth => BUTTERFLY_STAGE_2_DELAY + BUTTERFLY_CASCADE_DELAY + RED_DELAY - 1)
port map (
    clk => clk,
    ce => d.en,
    depth => BUTTERFLY_STAGE_2_DELAY + BUTTERFLY_CASCADE_DELAY + RED_DELAY - 1,
    d => d.a,
    q => delay_a_out
);

amux: process (d.inv, delay_a_out, d.b)
begin
    if d.inv = '0'
    then
        da.c(0) <= '0';
        da.c(23 downto 1) <= delay_a_out; -- a << 1
    else
        da.c(21 downto 0) <= d.b(22 downto 1); -- b >> 1
        da.c(23 downto 22) <= "00";
    end if;
end process;






-- REDUCER
reducer: entity work.bfu_reducer
port map (
    clk => clk,
    d => dr,
    q => qr
);

-- reducer inputs
dr.PCIN <= qa.PCOUT;
dr.sel <= qa.p(47);
dr.en <= d.en;







-- MODULAR REDUCTION
reduction: entity work.red_dsp
port map (
    clk => clk,
    d => redd,
    q => redq
);

-- mod reduction inputs
redd.en <= d.en;
redd.data <= q2.p(28 downto 0) & q1.p; 





-- delay adder result around reducer
delay_qap: entity work.dyn_shift_reg
generic map (width => 23, max_depth => BUTTERFLY_REDUCER_DELAY)
port map (
    clk => clk,
    ce => d.en,
    depth => BUTTERFLY_REDUCER_DELAY,
    d => qa.p(22 downto 0),
    q => qa_p_delayed
);


adder_pos <= qa_p_delayed when qr.p(47) = '1' else
             qr.p(22 downto 0);


-- delay output a: old with dyn shift reg, new with 2 shift regs that will be transformed to CLB
--delay_output_a_in <= redq.data when d.inv='0' else adder_pos;
--final_delay <= 3 when d.inv='0' else 9;
--delay_output_a: entity work.dyn_shift_reg
--generic map (width => 23, max_depth => 9)
--port map (
--    clk => clk,
--    ce => d.en,
--    depth => final_delay,
--    d => delay_output_a_in,
--    q => q.A
--);

delay_redq_data: entity work.dyn_shift_reg
generic map (width => 23, max_depth => 3)
port map (
    clk => clk,
    ce => d.en,
    depth => 3,
    d => redq.data,
    q => redq_data_delayed
);

delay_adder_pos: entity work.dyn_shift_reg
generic map (width => 23, max_depth => 9)
port map (
    clk => clk,
    ce => d.en,
    depth => 9,
    d => adder_pos,
    q => adder_pos_delayed
);

-- output muxes
q.A <= adder_pos_delayed when d.inv='1' else redq_data_delayed;
q.B <= redq.data when d.inv='1' else adder_pos;

end Behavioral;
