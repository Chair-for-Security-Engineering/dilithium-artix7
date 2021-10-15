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

entity counter is
    Generic (
        max_value : natural := 1023
    );
    Port (
        clk : in std_logic;
        d   : in counter_in_type;
        q   : out counter_out_type;
        value : out natural range 0 to max_value
    );
end counter;

architecture Behavioral of counter is

signal cnt, nextcnt : natural range 0 to max_value;

begin

value <= cnt;
nextcnt <= cnt + 1;
counter: process(clk)
begin
    if rising_edge(clk)
    then
      if d.rst = '1'
      then
        cnt <= 0;
        q.ovf <= '0';
        q.max <= '0';
      elsif q.ovf = '0' and d.en = '1'
      then
        q.max <= '0';
        if cnt = max_value - 1
        then
            q.max <= '1';
        end if;
        if cnt = max_value
        then
            q.ovf <= '1';
        else
            cnt <= nextcnt;
        end if;
      end if;
    end if;
end process;
      
end Behavioral;
