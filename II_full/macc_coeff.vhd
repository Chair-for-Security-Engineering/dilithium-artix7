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
USE IEEE.NUMERIC_STD.ALL;
    
Library UNISIM;
use UNISIM.vcomponents.all;

library work;
use work.dilithium.all;
use work.interfaces.all;

entity macc_coeff is
    Port (
        clk : std_logic;
        d   : in macc_coeff_in_type;
        q   : out macc_coeff_out_type
    );
end macc_coeff;

architecture Behavioral of macc_coeff is

    type red_in_array is array(0 to 3) of red_dsp_in_type;
    type red_out_array is array(0 to 3) of red_dsp_out_type;
    
    signal redd : red_in_array;
    signal redq : red_out_array;
    
    type macc_in_array is array(0 to 3) of macc_dsp_in_type;
    type macc_out_array is array(0 to 3) of std_logic_vector(45 downto 0);
    
    signal maccd : macc_in_array;
    signal maccq : macc_out_array;

begin

    redgen: for i in 0 to 3
    generate
        red: entity work.red_dsp
        port map (
            clk => clk,
            d => redd(i),
            q => redq(i)
        );
        redd(i).en <= d.en when d.op = op_macc else '0'; -- no need to reduce for addition
        redd(i).data <= maccq(i);
        q(i) <= redq(i).data when d.op = op_macc else maccq(i)(22 downto 0); 
    end generate;
    
    maccgen: for i in 0 to 3
    generate
        macc: entity work.macc_dsp
        port map (
            clk => clk,
            d => maccd(i),
            q => maccq(i)
        );
        maccd(i).en <= d.en;
        maccd(i).op <= d.op;
        maccd(i).a <= d.a(i);
        maccd(i).b <= d.b(i);
        maccd(i).c <= d.c(i);
    end generate;

end Behavioral;