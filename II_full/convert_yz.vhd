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

entity convert_yz is
    Port (
        clk : in std_logic;
        d : in convert_yz_in_type;
        q : out payload_array(0 to 3)
    );
end convert_yz;

architecture Behavioral of convert_yz is

begin

-- sample conversion
sampleconvgen: for i in 0 to 1
generate
    sampleconv: entity work.sample_y_dsp
    port map (
        clk => clk,
        en => d.en,
        sub => d.sub,
        d => d.data(i*2 to i*2+1),
        q => q(i*2 to i*2+1)
    );
end generate;

end Behavioral;
