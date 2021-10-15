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

-- Uncomment the following library declaration if using
-- arithmetic functions with Signed or Unsigned values
use IEEE.NUMERIC_STD.ALL;
use IEEE.STD_LOGIC_UNSIGNED.ALL;

-- Uncomment the following library declaration if instantiating
-- any Xilinx leaf cells in this code.
--library UNISIM;
--use UNISIM.VComponents.all;

entity red_dsp is
    Port (
        clk : in std_logic;
        d : in red_dsp_in_type;
        q : out red_dsp_out_type
    );
end red_dsp;

architecture Behavioral of red_dsp is

constant RED_STAGE_1_DELAY : integer := 4;
constant RED_STAGE_2_DELAY : integer := 3;
constant RED_STAGE_3_DELAY : integer := 1;
constant RED_STAGE_4_DELAY : integer := 1;

signal low_delayed : std_logic_vector(22 downto 0);

signal d1 : red_dsp_1_in_type;
signal d2 : red_dsp_2_in_type;
signal d3 : red_dsp_3_in_type;
signal d4 : red_dsp_4_in_type;
signal q1 : red_dsp_1_out_type;
signal q2 : red_dsp_2_out_type;
signal q3 : red_dsp_3_out_type;
signal q4 : red_dsp_4_out_type;

signal s1_lsb_add : std_logic_vector(1 downto 0);
signal s1_lsb : std_logic;

signal clow : std_logic_vector(9 downto 0);
signal e1 : std_logic_vector(9 downto 0);
signal e2 : std_logic_vector(1 downto 0);
signal stage_3_delayed : std_logic_vector(23 downto 0);

begin





-- STAGE 1 --
stage1: entity work.red_dsp_1
port map (
    clk => clk,
    d => d1,
    q => q1
);
d1.en <= d.en;

s1_lsb_add <= ("0"&d.data(43)) + ("0"&d.data(33)) + ("0"&d.data(23));

-- lower addition: we have not enough room, so we use the carry in and compute the lowest bit manually and delay this around stage 1
d1.cin <= s1_lsb_add(1);
d1.d(1 downto 0) <= d.data(45 downto 44);
d1.a(8 downto 0) <= d.data(42 downto 34);
d1.c(8 downto 0) <= d.data(32 downto 24);

-- leave two bits of space for the carries of the lower addition
d1.d(10 downto 2) <= (others => '0');
d1.a(10 downto 9) <= (others => '0');
d1.c(10 downto 9) <= (others => '0');

-- upper addition
d1.d(13 downto 11) <= d.data(45 downto 43);
d1.a(23 downto 11) <= d.data(45 downto 33);
d1.c(33 downto 11) <= d.data(45 downto 23);

-- delay lowest bit
delay_lsb: entity work.dyn_shift_reg
generic map (width => 1, max_depth => RED_STAGE_1_DELAY)
port map (
    clk => clk,
    ce => d.en,
    depth => RED_STAGE_1_DELAY,
    d(0) => s1_lsb_add(0),
    q(0) => s1_lsb
);

-- delay lower part of coefficient
delay_low: entity work.dyn_shift_reg
generic map (width => 23, max_depth => RED_STAGE_1_DELAY)
port map (
    clk => clk,
    ce => d.en,
    depth => RED_STAGE_1_DELAY,
    d => d.data(22 downto 0),
    q => low_delayed
);







-- STAGE 2 --
stage2: entity work.red_dsp_2
port map (
    clk => clk,
    d => d2,
    q => q2
);
d2.en <= d.en;
d2.d <= low_delayed; -- this is z[22:0]
d2.a <= q1.p(10 downto 9); -- this is c[11:10]
d2.c <= q1.p(34 downto 11); -- this is d



-- add c[11:10] to c[9:0]
clow(9 downto 1) <= q1.p(8 downto 0);
clow(0) <= s1_lsb;
addproc: process(clk)
begin
    if rising_edge(clk)
    then
        e1 <= clow;
        e2 <= q1.p(10 downto 9);
    end if;
end process;








-- STAGE 3 --
stage3: entity work.red_dsp_3
port map (
    clk => clk,
    d => d3,
    q => q3
);  
d3.e1 <= e1;
d3.e2 <= e2;
d3.pcin <= q2.pcout;
d3.en <= d.en;


delay_stage_3: entity work.dyn_shift_reg
generic map (width => 24, max_depth => RED_STAGE_4_DELAY)
port map (
    clk => clk,
    ce => d.en,
    depth => RED_STAGE_4_DELAY,
    d => q3.p(23 downto 0),
    q => stage_3_delayed
);






-- STAGE 4 --    
stage4: entity work.red_dsp_4
port map (
    clk  => clk,
    d => d4,
    q => q4
);
d4.en <= d.en;
d4.pcin <= q3.pcout;
d4.c <= std_logic_vector(to_signed(DILITHIUM_Q, 24)) when q3.p(47) = '1' else 
        std_logic_vector(to_signed(-DILITHIUM_Q, 24));

q.data <= stage_3_delayed(22 downto 0) when q4.p(47) = '1' else
          q4.p(22 downto 0);
        

assertproc: process(clk)
begin
    if rising_edge(clk)
    then
        assert to_integer(unsigned(q.data)) < DILITHIUM_Q report "reduction failed: "&integer'image(to_integer(unsigned(q.data))) severity warning;
    end if;
end process;

end Behavioral;
