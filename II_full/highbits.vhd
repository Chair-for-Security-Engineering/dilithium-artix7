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

entity highbits is
    Port(
        clk : in std_logic;
        d   : in std_logic_vector(22 downto 0);
        q   : out std_logic_vector(22 downto 0)
    );
end highbits;

architecture Behavioral of highbits is

    signal qtmp : std_logic_vector(5 downto 0);

begin

q(22 downto 6) <= (others => '0');

qreg: process(clk)
begin
    if rising_edge(clk)
    then
        q(5 downto 0) <= qtmp;
    end if;
end process;

hb_88: if DILITHIUM_gamma2 = (DILITHIUM_Q-1)/88
generate
    qtmp(5 downto 0) <= 
            "000000" when unsigned(d) <   95232 else
            "000001" when unsigned(d) <  285696 else
            "000010" when unsigned(d) <  476160 else
            "000011" when unsigned(d) <  666624 else
            "000100" when unsigned(d) <  857088 else
            "000101" when unsigned(d) < 1047552 else
            "000110" when unsigned(d) < 1238016 else
            "000111" when unsigned(d) < 1428480 else
            "001000" when unsigned(d) < 1618944 else
            "001001" when unsigned(d) < 1809408 else
            "001010" when unsigned(d) < 1999872 else
            "001011" when unsigned(d) < 2190336 else
            "001100" when unsigned(d) < 2380800 else
            "001101" when unsigned(d) < 2571264 else
            "001110" when unsigned(d) < 2761728 else
            "001111" when unsigned(d) < 2952192 else
            "010000" when unsigned(d) < 3142656 else
            "010001" when unsigned(d) < 3333120 else
            "010010" when unsigned(d) < 3523584 else
            "010011" when unsigned(d) < 3714048 else
            "010100" when unsigned(d) < 3904512 else
            "010101" when unsigned(d) < 4094976 else
            "010110" when unsigned(d) < 4285440 else
            "010111" when unsigned(d) < 4475904 else
            "011000" when unsigned(d) < 4666368 else
            "011001" when unsigned(d) < 4856832 else
            "011010" when unsigned(d) < 5047296 else
            "011011" when unsigned(d) < 5237760 else
            "011100" when unsigned(d) < 5428224 else
            "011101" when unsigned(d) < 5618688 else
            "011110" when unsigned(d) < 5809152 else
            "011111" when unsigned(d) < 5999616 else
            "100000" when unsigned(d) < 6190080 else
            "100001" when unsigned(d) < 6380544 else
            "100010" when unsigned(d) < 6571008 else
            "100011" when unsigned(d) < 6761472 else
            "100100" when unsigned(d) < 6951936 else
            "100101" when unsigned(d) < 7142400 else
            "100110" when unsigned(d) < 7332864 else
            "100111" when unsigned(d) < 7523328 else
            "101000" when unsigned(d) < 7713792 else
            "101001" when unsigned(d) < 7904256 else
            "101010" when unsigned(d) < 8094720 else
            "101011" when unsigned(d) < 8285184 else
            "000000";
end generate;
hb_32: if DILITHIUM_gamma2 = (DILITHIUM_Q-1)/32
generate
    qtmp(5 downto 4) <= "00";
    qtmp(3 downto 0) <= 
            "0000" when unsigned(d) <  261888 else
            "0001" when unsigned(d) <  785664 else
            "0010" when unsigned(d) < 1309440 else
            "0011" when unsigned(d) < 1833216 else
            "0100" when unsigned(d) < 2356992 else
            "0101" when unsigned(d) < 2880768 else
            "0110" when unsigned(d) < 3404544 else
            "0111" when unsigned(d) < 3928320 else
            "1000" when unsigned(d) < 4452096 else
            "1001" when unsigned(d) < 4975872 else
            "1010" when unsigned(d) < 5499648 else
            "1011" when unsigned(d) < 6023424 else
            "1100" when unsigned(d) < 6547200 else
            "1101" when unsigned(d) < 7070976 else
            "1110" when unsigned(d) < 7594752 else
            "1111" when unsigned(d) < 8118528 else
            "0000";
end generate;
end Behavioral;
