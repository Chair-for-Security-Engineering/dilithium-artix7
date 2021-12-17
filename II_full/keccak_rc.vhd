-- COPYRIGHT (c) 2021 ALL RIGHT RESERVED
-- Chair for Security Engineering
-- Georg Land (georg.land@rub.de)
-- License: see LICENSE file

-- THIS CODE AND INFORMATION ARE PROVIDED "AS IS" WITHOUT WARRANTY OF ANY 
-- KIND, EITHER EXPRESSED OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE
-- IMPLIED WARRANTIES OF MERCHANTABILITY AND/OR FITNESS FOR A
-- PARTICULAR PURPOSE.

----------------------------------------------------------------------------------
-- COPYRIGHT (c) 2018 ALL RIGHT RESERVED
--
-- COMPANY:					Ruhr-University Bochum, Chair for Security Engineering
-- AUTHOR:					Jan Richter-Brockmann
--
-- CREATE DATE:			    13/12/2018
-- LAST CHANGES:            13/12/2018
-- MODULE NAME:			    KECCAK_RC
--
-- REVISION:				1.00 - Contains a LUT for the KECCAK round constants
--
-- LICENCE: 				Please look at licence.txt
-- USAGE INFORMATION:	    Please look at readme.txt. If licence.txt or readme.txt
--							are missing or if you have questions regarding the code
--							please contact Tim G�neysu (tim.gueneysu@rub.de) and
--                          Jan Richter-Brockmann (jan.richter-brockmann@rub.de)
--
-- THIS CODE AND INFORMATION ARE PROVIDED "AS IS" WITHOUT WARRANTY OF ANY 
-- KIND, EITHER EXPRESSED OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE
-- IMPLIED WARRANTIES OF MERCHANTABILITY AND/OR FITNESS FOR A
-- PARTICULAR PURPOSE.
----------------------------------------------------------------------------------


LIBRARY IEEE;
    USE IEEE.STD_LOGIC_1164.ALL;
    
LIBRARY work;
        USE work.keccak_settings.ALL;



ENTITY KECCAK_RC IS
    PORT ( round        : IN  STD_LOGIC_VECTOR(4 DOWNTO 0);
           const_out    : OUT STD_LOGIC_VECTOR(LANE_WIDTH-1 DOWNTO 0));
END KECCAK_RC;



ARCHITECTURE Behavioral OF KECCAK_RC IS



-- SIGNALS -----------------------------------------------------------------------
SIGNAL const_signal : STD_LOGIC_VECTOR(63 DOWNTO 0);



-- BEHAVIORAL --------------------------------------------------------------------
BEGIN

    RC : PROCESS (round)
    BEGIN
      CASE round IS
        WHEN "00000" => const_signal <= X"0000000000000001" ;
        WHEN "00001" => const_signal <= X"0000000000008082" ;
        WHEN "00010" => const_signal <= X"800000000000808A" ;
        WHEN "00011" => const_signal <= X"8000000080008000" ;
        WHEN "00100" => const_signal <= X"000000000000808B" ;
        WHEN "00101" => const_signal <= X"0000000080000001" ;
        WHEN "00110" => const_signal <= X"8000000080008081" ;
        WHEN "00111" => const_signal <= X"8000000000008009" ;
        WHEN "01000" => const_signal <= X"000000000000008A" ;
        WHEN "01001" => const_signal <= X"0000000000000088" ;
        WHEN "01010" => const_signal <= X"0000000080008009" ;
        WHEN "01011" => const_signal <= X"000000008000000A" ;
        WHEN "01100" => const_signal <= X"000000008000808B" ;
        WHEN "01101" => const_signal <= X"800000000000008B" ;
        WHEN "01110" => const_signal <= X"8000000000008089" ;
        WHEN "01111" => const_signal <= X"8000000000008003" ;
        WHEN "10000" => const_signal <= X"8000000000008002" ;
        WHEN "10001" => const_signal <= X"8000000000000080" ;
        WHEN "10010" => const_signal <= X"000000000000800A" ;
        WHEN "10011" => const_signal <= X"800000008000000A" ;
        WHEN "10100" => const_signal <= X"8000000080008081" ;
        WHEN "10101" => const_signal <= X"8000000000008080" ;
        WHEN "10110" => const_signal <= X"0000000080000001" ;
        WHEN "10111" => const_signal <= X"8000000080008008" ;                    
        WHEN OTHERS => const_signal <=(OTHERS => '0');
      END CASE;
    END PROCESS RC;
    
    const_out <= const_signal(LANE_WIDTH-1 DOWNTO 0);

END Behavioral;
