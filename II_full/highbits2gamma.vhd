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

entity highbits2gamma is
    Port (
        d : in payload_array(0 to 3);
        q : out payload_array(0 to 3)
    );
end highbits2gamma;

architecture Behavioral of highbits2gamma is

begin

lut0gen: if DILITHIUM_gamma2 = (DILITHIUM_Q-1)/88
generate
    lut0genloop: for i in 0 to 3
    generate
        q(i) <= std_logic_vector(to_unsigned(      0, 23)) when unsigned(d(i)) <   95232 else
                std_logic_vector(to_unsigned( 190464, 23)) when unsigned(d(i)) <  285696 else
                std_logic_vector(to_unsigned( 380928, 23)) when unsigned(d(i)) <  476160 else
                std_logic_vector(to_unsigned( 571392, 23)) when unsigned(d(i)) <  666624 else
                std_logic_vector(to_unsigned( 761856, 23)) when unsigned(d(i)) <  857088 else
                std_logic_vector(to_unsigned( 952320, 23)) when unsigned(d(i)) < 1047552 else
                std_logic_vector(to_unsigned(1142784, 23)) when unsigned(d(i)) < 1238016 else
                std_logic_vector(to_unsigned(1333248, 23)) when unsigned(d(i)) < 1428480 else
                std_logic_vector(to_unsigned(1523712, 23)) when unsigned(d(i)) < 1618944 else
                std_logic_vector(to_unsigned(1714176, 23)) when unsigned(d(i)) < 1809408 else
                std_logic_vector(to_unsigned(1904640, 23)) when unsigned(d(i)) < 1999872 else
                std_logic_vector(to_unsigned(2095104, 23)) when unsigned(d(i)) < 2190336 else
                std_logic_vector(to_unsigned(2285568, 23)) when unsigned(d(i)) < 2380800 else
                std_logic_vector(to_unsigned(2476032, 23)) when unsigned(d(i)) < 2571264 else
                std_logic_vector(to_unsigned(2666496, 23)) when unsigned(d(i)) < 2761728 else
                std_logic_vector(to_unsigned(2856960, 23)) when unsigned(d(i)) < 2952192 else
                std_logic_vector(to_unsigned(3047424, 23)) when unsigned(d(i)) < 3142656 else
                std_logic_vector(to_unsigned(3237888, 23)) when unsigned(d(i)) < 3333120 else
                std_logic_vector(to_unsigned(3428352, 23)) when unsigned(d(i)) < 3523584 else
                std_logic_vector(to_unsigned(3618816, 23)) when unsigned(d(i)) < 3714048 else
                std_logic_vector(to_unsigned(3809280, 23)) when unsigned(d(i)) < 3904512 else
                std_logic_vector(to_unsigned(3999744, 23)) when unsigned(d(i)) < 4094976 else
                std_logic_vector(to_unsigned(4190208, 23)) when unsigned(d(i)) < 4285440 else
                std_logic_vector(to_unsigned(4380672, 23)) when unsigned(d(i)) < 4475904 else
                std_logic_vector(to_unsigned(4571136, 23)) when unsigned(d(i)) < 4666368 else
                std_logic_vector(to_unsigned(4761600, 23)) when unsigned(d(i)) < 4856832 else
                std_logic_vector(to_unsigned(4952064, 23)) when unsigned(d(i)) < 5047296 else
                std_logic_vector(to_unsigned(5142528, 23)) when unsigned(d(i)) < 5237760 else
                std_logic_vector(to_unsigned(5332992, 23)) when unsigned(d(i)) < 5428224 else
                std_logic_vector(to_unsigned(5523456, 23)) when unsigned(d(i)) < 5618688 else
                std_logic_vector(to_unsigned(5713920, 23)) when unsigned(d(i)) < 5809152 else
                std_logic_vector(to_unsigned(5904384, 23)) when unsigned(d(i)) < 5999616 else
                std_logic_vector(to_unsigned(6094848, 23)) when unsigned(d(i)) < 6190080 else
                std_logic_vector(to_unsigned(6285312, 23)) when unsigned(d(i)) < 6380544 else
                std_logic_vector(to_unsigned(6475776, 23)) when unsigned(d(i)) < 6571008 else
                std_logic_vector(to_unsigned(6666240, 23)) when unsigned(d(i)) < 6761472 else
                std_logic_vector(to_unsigned(6856704, 23)) when unsigned(d(i)) < 6951936 else
                std_logic_vector(to_unsigned(7047168, 23)) when unsigned(d(i)) < 7142400 else
                std_logic_vector(to_unsigned(7237632, 23)) when unsigned(d(i)) < 7332864 else
                std_logic_vector(to_unsigned(7428096, 23)) when unsigned(d(i)) < 7523328 else
                std_logic_vector(to_unsigned(7618560, 23)) when unsigned(d(i)) < 7713792 else
                std_logic_vector(to_unsigned(7809024, 23)) when unsigned(d(i)) < 7904256 else
                std_logic_vector(to_unsigned(7999488, 23)) when unsigned(d(i)) < 8094720 else
                std_logic_vector(to_unsigned(8189952, 23)) when unsigned(d(i)) < 8285184 else
                std_logic_vector(to_unsigned(      0, 23));
    end generate;
end generate;
lut1gen: if DILITHIUM_gamma2 = (DILITHIUM_Q-1)/32
generate
    lut1genloop: for i in 0 to 3
    generate
        q(i) <= std_logic_vector(to_unsigned(      0, 23)) when unsigned(d(i)) <  261888 else
                std_logic_vector(to_unsigned( 523776, 23)) when unsigned(d(i)) <  785664 else
                std_logic_vector(to_unsigned(1047552, 23)) when unsigned(d(i)) < 1309440 else
                std_logic_vector(to_unsigned(1571328, 23)) when unsigned(d(i)) < 1833216 else
                std_logic_vector(to_unsigned(2095104, 23)) when unsigned(d(i)) < 2356992 else
                std_logic_vector(to_unsigned(2618880, 23)) when unsigned(d(i)) < 2880768 else
                std_logic_vector(to_unsigned(3142656, 23)) when unsigned(d(i)) < 3404544 else
                std_logic_vector(to_unsigned(3666432, 23)) when unsigned(d(i)) < 3928320 else
                std_logic_vector(to_unsigned(4190208, 23)) when unsigned(d(i)) < 4452096 else
                std_logic_vector(to_unsigned(4713984, 23)) when unsigned(d(i)) < 4975872 else
                std_logic_vector(to_unsigned(5237760, 23)) when unsigned(d(i)) < 5499648 else
                std_logic_vector(to_unsigned(5761536, 23)) when unsigned(d(i)) < 6023424 else
                std_logic_vector(to_unsigned(6285312, 23)) when unsigned(d(i)) < 6547200 else
                std_logic_vector(to_unsigned(6809088, 23)) when unsigned(d(i)) < 7070976 else
                std_logic_vector(to_unsigned(7332864, 23)) when unsigned(d(i)) < 7594752 else
                std_logic_vector(to_unsigned(7856640, 23)) when unsigned(d(i)) < 8118528 else
                std_logic_vector(to_unsigned(      0, 23));
    end generate;
end generate;

end Behavioral;
