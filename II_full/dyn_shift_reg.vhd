-- COPYRIGHT (c) 2021 ALL RIGHT RESERVED
-- Chair for Security Engineering
-- Georg Land (georg.land@rub.de)
-- License: see LICENSE file

-- THIS CODE AND INFORMATION ARE PROVIDED "AS IS" WITHOUT WARRANTY OF ANY 
-- KIND, EITHER EXPRESSED OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE
-- IMPLIED WARRANTIES OF MERCHANTABILITY AND/OR FITNESS FOR A
-- PARTICULAR PURPOSE.

library IEEE;
use IEEE.STD_LOGIC_1164.all;
use IEEE.NUMERIC_STD.ALL;

entity dyn_shift_reg is
  Generic (
    width     : natural := 10;
    max_depth : natural := 255
  );
  Port (
      clk   : in std_logic;
      ce    : in std_logic;
      depth : in natural range 1 to max_depth;
      d     : in  std_logic_vector(width-1 downto 0) := (others => '0');
      q     : out std_logic_vector(width-1 downto 0) := (others => '0')
  );
end dyn_shift_reg;

architecture Behavioral of dyn_shift_reg is
  
  type storage_type is array (max_depth downto 0) of std_logic_vector(width-1 downto 0);
  signal storage : storage_type := (others => (others => '0'));
  
begin

q <= d when max_depth = 0 else storage(depth-1) when depth >= 1 else (others => '0');

reg: process(clk)
begin
    if rising_edge(clk)
    then
        if ce = '1'
        then
            for i in 0 to max_depth-1 loop
                storage(i+1) <= storage(i);
            end loop;
            storage(0) <= d;
        end if;
    end if;
end process;

end Behavioral;

