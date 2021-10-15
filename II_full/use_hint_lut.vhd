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

entity use_hint_lut is
    Port (
        clk : in std_logic;
        d : in std_logic_vector(22 downto 0);
        q0 : out std_logic_vector(22 downto 0);
        q1 : out std_logic_vector(22 downto 0)
    );
end use_hint_lut;

architecture Behavioral of use_hint_lut is

signal q0tmp, q1tmp : std_logic_vector(5 downto 0);

begin


lutgen0: if DILITHIUM_gamma2 = (DILITHIUM_Q-1)/32
generate
    
    q0(22 downto 4) <= (others => '0');
    q1(22 downto 4) <= (others => '0');
    
    qreg: process(clk)
    begin
        if rising_edge(clk)
        then
            q0(3 downto 0) <= q0tmp(3 downto 0);
            q1(3 downto 0) <= q1tmp(3 downto 0);
        end if;
    end process;
    q0tmp(3 downto 0) <= 
        std_logic_vector(to_unsigned( 0, 4)) when unsigned(d) <  261889 else
        std_logic_vector(to_unsigned( 1, 4)) when unsigned(d) <  785665 else
        std_logic_vector(to_unsigned( 2, 4)) when unsigned(d) < 1309441 else
        std_logic_vector(to_unsigned( 3, 4)) when unsigned(d) < 1833217 else
        std_logic_vector(to_unsigned( 4, 4)) when unsigned(d) < 2356993 else
        std_logic_vector(to_unsigned( 5, 4)) when unsigned(d) < 2880769 else
        std_logic_vector(to_unsigned( 6, 4)) when unsigned(d) < 3404545 else
        std_logic_vector(to_unsigned( 7, 4)) when unsigned(d) < 3928321 else
        std_logic_vector(to_unsigned( 8, 4)) when unsigned(d) < 4452097 else
        std_logic_vector(to_unsigned( 9, 4)) when unsigned(d) < 4975873 else
        std_logic_vector(to_unsigned(10, 4)) when unsigned(d) < 5499649 else
        std_logic_vector(to_unsigned(11, 4)) when unsigned(d) < 6023425 else
        std_logic_vector(to_unsigned(12, 4)) when unsigned(d) < 6547201 else
        std_logic_vector(to_unsigned(13, 4)) when unsigned(d) < 7070977 else
        std_logic_vector(to_unsigned(14, 4)) when unsigned(d) < 7594753 else
        std_logic_vector(to_unsigned(15, 4)) when unsigned(d) < 8118529 else
        std_logic_vector(to_unsigned( 0, 4));
    q1tmp(3 downto 0) <= 
        std_logic_vector(to_unsigned(15, 4)) when unsigned(d) <       1 else
        std_logic_vector(to_unsigned( 1, 4)) when unsigned(d) <  261889 else
        std_logic_vector(to_unsigned( 0, 4)) when unsigned(d) <  523777 else
        std_logic_vector(to_unsigned( 2, 4)) when unsigned(d) <  785665 else
        std_logic_vector(to_unsigned( 1, 4)) when unsigned(d) < 1047553 else
        std_logic_vector(to_unsigned( 3, 4)) when unsigned(d) < 1309441 else
        std_logic_vector(to_unsigned( 2, 4)) when unsigned(d) < 1571329 else
        std_logic_vector(to_unsigned( 4, 4)) when unsigned(d) < 1833217 else
        std_logic_vector(to_unsigned( 3, 4)) when unsigned(d) < 2095105 else
        std_logic_vector(to_unsigned( 5, 4)) when unsigned(d) < 2356993 else
        std_logic_vector(to_unsigned( 4, 4)) when unsigned(d) < 2618881 else
        std_logic_vector(to_unsigned( 6, 4)) when unsigned(d) < 2880769 else
        std_logic_vector(to_unsigned( 5, 4)) when unsigned(d) < 3142657 else
        std_logic_vector(to_unsigned( 7, 4)) when unsigned(d) < 3404545 else
        std_logic_vector(to_unsigned( 6, 4)) when unsigned(d) < 3666433 else
        std_logic_vector(to_unsigned( 8, 4)) when unsigned(d) < 3928321 else
        std_logic_vector(to_unsigned( 7, 4)) when unsigned(d) < 4190209 else
        std_logic_vector(to_unsigned( 9, 4)) when unsigned(d) < 4452097 else
        std_logic_vector(to_unsigned( 8, 4)) when unsigned(d) < 4713985 else
        std_logic_vector(to_unsigned(10, 4)) when unsigned(d) < 4975873 else
        std_logic_vector(to_unsigned( 9, 4)) when unsigned(d) < 5237761 else
        std_logic_vector(to_unsigned(11, 4)) when unsigned(d) < 5499649 else
        std_logic_vector(to_unsigned(10, 4)) when unsigned(d) < 5761537 else
        std_logic_vector(to_unsigned(12, 4)) when unsigned(d) < 6023425 else
        std_logic_vector(to_unsigned(11, 4)) when unsigned(d) < 6285313 else
        std_logic_vector(to_unsigned(13, 4)) when unsigned(d) < 6547201 else
        std_logic_vector(to_unsigned(12, 4)) when unsigned(d) < 6809089 else
        std_logic_vector(to_unsigned(14, 4)) when unsigned(d) < 7070977 else
        std_logic_vector(to_unsigned(13, 4)) when unsigned(d) < 7332865 else
        std_logic_vector(to_unsigned(15, 4)) when unsigned(d) < 7594753 else
        std_logic_vector(to_unsigned(14, 4)) when unsigned(d) < 7856641 else
        std_logic_vector(to_unsigned( 0, 4)) when unsigned(d) < 8118529 else
        std_logic_vector(to_unsigned(15, 4));
end generate;



lutgen1: if DILITHIUM_gamma2 = (DILITHIUM_Q-1)/88
generate
    
    q0(22 downto 6) <= (others => '0');
    q1(22 downto 6) <= (others => '0');
    
    qreg: process(clk)
    begin
        if rising_edge(clk)
        then
            q0(5 downto 0) <= q0tmp;
            q1(5 downto 0) <= q1tmp;
        end if;
    end process;
    q0tmp <= 
        std_logic_vector(to_unsigned( 0, 6)) when unsigned(d) <   95233 else
        std_logic_vector(to_unsigned( 1, 6)) when unsigned(d) <  285697 else
        std_logic_vector(to_unsigned( 2, 6)) when unsigned(d) <  476161 else
        std_logic_vector(to_unsigned( 3, 6)) when unsigned(d) <  666625 else
        std_logic_vector(to_unsigned( 4, 6)) when unsigned(d) <  857089 else
        std_logic_vector(to_unsigned( 5, 6)) when unsigned(d) < 1047553 else
        std_logic_vector(to_unsigned( 6, 6)) when unsigned(d) < 1238017 else
        std_logic_vector(to_unsigned( 7, 6)) when unsigned(d) < 1428481 else
        std_logic_vector(to_unsigned( 8, 6)) when unsigned(d) < 1618945 else
        std_logic_vector(to_unsigned( 9, 6)) when unsigned(d) < 1809409 else
        std_logic_vector(to_unsigned(10, 6)) when unsigned(d) < 1999873 else
        std_logic_vector(to_unsigned(11, 6)) when unsigned(d) < 2190337 else
        std_logic_vector(to_unsigned(12, 6)) when unsigned(d) < 2380801 else
        std_logic_vector(to_unsigned(13, 6)) when unsigned(d) < 2571265 else
        std_logic_vector(to_unsigned(14, 6)) when unsigned(d) < 2761729 else
        std_logic_vector(to_unsigned(15, 6)) when unsigned(d) < 2952193 else
        std_logic_vector(to_unsigned(16, 6)) when unsigned(d) < 3142657 else
        std_logic_vector(to_unsigned(17, 6)) when unsigned(d) < 3333121 else
        std_logic_vector(to_unsigned(18, 6)) when unsigned(d) < 3523585 else
        std_logic_vector(to_unsigned(19, 6)) when unsigned(d) < 3714049 else
        std_logic_vector(to_unsigned(20, 6)) when unsigned(d) < 3904513 else
        std_logic_vector(to_unsigned(21, 6)) when unsigned(d) < 4094977 else
        std_logic_vector(to_unsigned(22, 6)) when unsigned(d) < 4285441 else
        std_logic_vector(to_unsigned(23, 6)) when unsigned(d) < 4475905 else
        std_logic_vector(to_unsigned(24, 6)) when unsigned(d) < 4666369 else
        std_logic_vector(to_unsigned(25, 6)) when unsigned(d) < 4856833 else
        std_logic_vector(to_unsigned(26, 6)) when unsigned(d) < 5047297 else
        std_logic_vector(to_unsigned(27, 6)) when unsigned(d) < 5237761 else
        std_logic_vector(to_unsigned(28, 6)) when unsigned(d) < 5428225 else
        std_logic_vector(to_unsigned(29, 6)) when unsigned(d) < 5618689 else
        std_logic_vector(to_unsigned(30, 6)) when unsigned(d) < 5809153 else
        std_logic_vector(to_unsigned(31, 6)) when unsigned(d) < 5999617 else
        std_logic_vector(to_unsigned(32, 6)) when unsigned(d) < 6190081 else
        std_logic_vector(to_unsigned(33, 6)) when unsigned(d) < 6380545 else
        std_logic_vector(to_unsigned(34, 6)) when unsigned(d) < 6571009 else
        std_logic_vector(to_unsigned(35, 6)) when unsigned(d) < 6761473 else
        std_logic_vector(to_unsigned(36, 6)) when unsigned(d) < 6951937 else
        std_logic_vector(to_unsigned(37, 6)) when unsigned(d) < 7142401 else
        std_logic_vector(to_unsigned(38, 6)) when unsigned(d) < 7332865 else
        std_logic_vector(to_unsigned(39, 6)) when unsigned(d) < 7523329 else
        std_logic_vector(to_unsigned(40, 6)) when unsigned(d) < 7713793 else
        std_logic_vector(to_unsigned(41, 6)) when unsigned(d) < 7904257 else
        std_logic_vector(to_unsigned(42, 6)) when unsigned(d) < 8094721 else
        std_logic_vector(to_unsigned(43, 6)) when unsigned(d) < 8285185 else
        std_logic_vector(to_unsigned( 0, 6));
q1tmp <= 
        std_logic_vector(to_unsigned(43, 6)) when unsigned(d) <       1 else
        std_logic_vector(to_unsigned( 1, 6)) when unsigned(d) <   95233 else
        std_logic_vector(to_unsigned( 0, 6)) when unsigned(d) <  190465 else
        std_logic_vector(to_unsigned( 2, 6)) when unsigned(d) <  285697 else
        std_logic_vector(to_unsigned( 1, 6)) when unsigned(d) <  380929 else
        std_logic_vector(to_unsigned( 3, 6)) when unsigned(d) <  476161 else
        std_logic_vector(to_unsigned( 2, 6)) when unsigned(d) <  571393 else
        std_logic_vector(to_unsigned( 4, 6)) when unsigned(d) <  666625 else
        std_logic_vector(to_unsigned( 3, 6)) when unsigned(d) <  761857 else
        std_logic_vector(to_unsigned( 5, 6)) when unsigned(d) <  857089 else
        std_logic_vector(to_unsigned( 4, 6)) when unsigned(d) <  952321 else
        std_logic_vector(to_unsigned( 6, 6)) when unsigned(d) < 1047553 else
        std_logic_vector(to_unsigned( 5, 6)) when unsigned(d) < 1142785 else
        std_logic_vector(to_unsigned( 7, 6)) when unsigned(d) < 1238017 else
        std_logic_vector(to_unsigned( 6, 6)) when unsigned(d) < 1333249 else
        std_logic_vector(to_unsigned( 8, 6)) when unsigned(d) < 1428481 else
        std_logic_vector(to_unsigned( 7, 6)) when unsigned(d) < 1523713 else
        std_logic_vector(to_unsigned( 9, 6)) when unsigned(d) < 1618945 else
        std_logic_vector(to_unsigned( 8, 6)) when unsigned(d) < 1714177 else
        std_logic_vector(to_unsigned(10, 6)) when unsigned(d) < 1809409 else
        std_logic_vector(to_unsigned( 9, 6)) when unsigned(d) < 1904641 else
        std_logic_vector(to_unsigned(11, 6)) when unsigned(d) < 1999873 else
        std_logic_vector(to_unsigned(10, 6)) when unsigned(d) < 2095105 else
        std_logic_vector(to_unsigned(12, 6)) when unsigned(d) < 2190337 else
        std_logic_vector(to_unsigned(11, 6)) when unsigned(d) < 2285569 else
        std_logic_vector(to_unsigned(13, 6)) when unsigned(d) < 2380801 else
        std_logic_vector(to_unsigned(12, 6)) when unsigned(d) < 2476033 else
        std_logic_vector(to_unsigned(14, 6)) when unsigned(d) < 2571265 else
        std_logic_vector(to_unsigned(13, 6)) when unsigned(d) < 2666497 else
        std_logic_vector(to_unsigned(15, 6)) when unsigned(d) < 2761729 else
        std_logic_vector(to_unsigned(14, 6)) when unsigned(d) < 2856961 else
        std_logic_vector(to_unsigned(16, 6)) when unsigned(d) < 2952193 else
        std_logic_vector(to_unsigned(15, 6)) when unsigned(d) < 3047425 else
        std_logic_vector(to_unsigned(17, 6)) when unsigned(d) < 3142657 else
        std_logic_vector(to_unsigned(16, 6)) when unsigned(d) < 3237889 else
        std_logic_vector(to_unsigned(18, 6)) when unsigned(d) < 3333121 else
        std_logic_vector(to_unsigned(17, 6)) when unsigned(d) < 3428353 else
        std_logic_vector(to_unsigned(19, 6)) when unsigned(d) < 3523585 else
        std_logic_vector(to_unsigned(18, 6)) when unsigned(d) < 3618817 else
        std_logic_vector(to_unsigned(20, 6)) when unsigned(d) < 3714049 else
        std_logic_vector(to_unsigned(19, 6)) when unsigned(d) < 3809281 else
        std_logic_vector(to_unsigned(21, 6)) when unsigned(d) < 3904513 else
        std_logic_vector(to_unsigned(20, 6)) when unsigned(d) < 3999745 else
        std_logic_vector(to_unsigned(22, 6)) when unsigned(d) < 4094977 else
        std_logic_vector(to_unsigned(21, 6)) when unsigned(d) < 4190209 else
        std_logic_vector(to_unsigned(23, 6)) when unsigned(d) < 4285441 else
        std_logic_vector(to_unsigned(22, 6)) when unsigned(d) < 4380673 else
        std_logic_vector(to_unsigned(24, 6)) when unsigned(d) < 4475905 else
        std_logic_vector(to_unsigned(23, 6)) when unsigned(d) < 4571137 else
        std_logic_vector(to_unsigned(25, 6)) when unsigned(d) < 4666369 else
        std_logic_vector(to_unsigned(24, 6)) when unsigned(d) < 4761601 else
        std_logic_vector(to_unsigned(26, 6)) when unsigned(d) < 4856833 else
        std_logic_vector(to_unsigned(25, 6)) when unsigned(d) < 4952065 else
        std_logic_vector(to_unsigned(27, 6)) when unsigned(d) < 5047297 else
        std_logic_vector(to_unsigned(26, 6)) when unsigned(d) < 5142529 else
        std_logic_vector(to_unsigned(28, 6)) when unsigned(d) < 5237761 else
        std_logic_vector(to_unsigned(27, 6)) when unsigned(d) < 5332993 else
        std_logic_vector(to_unsigned(29, 6)) when unsigned(d) < 5428225 else
        std_logic_vector(to_unsigned(28, 6)) when unsigned(d) < 5523457 else
        std_logic_vector(to_unsigned(30, 6)) when unsigned(d) < 5618689 else
        std_logic_vector(to_unsigned(29, 6)) when unsigned(d) < 5713921 else
        std_logic_vector(to_unsigned(31, 6)) when unsigned(d) < 5809153 else
        std_logic_vector(to_unsigned(30, 6)) when unsigned(d) < 5904385 else
        std_logic_vector(to_unsigned(32, 6)) when unsigned(d) < 5999617 else
        std_logic_vector(to_unsigned(31, 6)) when unsigned(d) < 6094849 else
        std_logic_vector(to_unsigned(33, 6)) when unsigned(d) < 6190081 else
        std_logic_vector(to_unsigned(32, 6)) when unsigned(d) < 6285313 else
        std_logic_vector(to_unsigned(34, 6)) when unsigned(d) < 6380545 else
        std_logic_vector(to_unsigned(33, 6)) when unsigned(d) < 6475777 else
        std_logic_vector(to_unsigned(35, 6)) when unsigned(d) < 6571009 else
        std_logic_vector(to_unsigned(34, 6)) when unsigned(d) < 6666241 else
        std_logic_vector(to_unsigned(36, 6)) when unsigned(d) < 6761473 else
        std_logic_vector(to_unsigned(35, 6)) when unsigned(d) < 6856705 else
        std_logic_vector(to_unsigned(37, 6)) when unsigned(d) < 6951937 else
        std_logic_vector(to_unsigned(36, 6)) when unsigned(d) < 7047169 else
        std_logic_vector(to_unsigned(38, 6)) when unsigned(d) < 7142401 else
        std_logic_vector(to_unsigned(37, 6)) when unsigned(d) < 7237633 else
        std_logic_vector(to_unsigned(39, 6)) when unsigned(d) < 7332865 else
        std_logic_vector(to_unsigned(38, 6)) when unsigned(d) < 7428097 else
        std_logic_vector(to_unsigned(40, 6)) when unsigned(d) < 7523329 else
        std_logic_vector(to_unsigned(39, 6)) when unsigned(d) < 7618561 else
        std_logic_vector(to_unsigned(41, 6)) when unsigned(d) < 7713793 else
        std_logic_vector(to_unsigned(40, 6)) when unsigned(d) < 7809025 else
        std_logic_vector(to_unsigned(42, 6)) when unsigned(d) < 7904257 else
        std_logic_vector(to_unsigned(41, 6)) when unsigned(d) < 7999489 else
        std_logic_vector(to_unsigned(43, 6)) when unsigned(d) < 8094721 else
        std_logic_vector(to_unsigned(42, 6)) when unsigned(d) < 8189953 else
        std_logic_vector(to_unsigned( 0, 6)) when unsigned(d) < 8285185 else
        std_logic_vector(to_unsigned(43, 6));
end generate;
end Behavioral;
