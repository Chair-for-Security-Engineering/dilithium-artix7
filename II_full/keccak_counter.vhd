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
-- MODULE NAME:			    KECCAK_COUNTER
--
-- REVISION:				1.00 - File created: counter
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
    USE IEEE.NUMERIC_STD.ALL;
    
LIBRARY work;
    USE work.keccak_settings.ALL;
        

ENTITY KECCAK_COUNTER IS
    GENERIC (   SIZE        : POSITIVE := 5;
                MAX_VALUE   : INTEGER := 20);
    PORT(       CLK         : IN  STD_LOGIC;
                EN          : IN  STD_LOGIC;
                RST         : IN  STD_LOGIC;
                CNT_OUT     : OUT STD_LOGIC_VECTOR((SIZE-1) DOWNTO 0));
END KECCAK_COUNTER;



-- ARCHITECTURE ------------------------------------------------------------------
ARCHITECTURE Behavioral OF KECCAK_COUNTER IS



-- SIGNALS -----------------------------------------------------------------------
SIGNAL COUNT    : UNSIGNED((SIZE-1) DOWNTO 0) := (OTHERS => '0');
SIGNAL COUNT_IN : UNSIGNED((SIZE-1) DOWNTO 0);



-- BEHAVIORAL ---------------------------------------------------------------------
BEGIN

    -- COUNTER PROCESS ------------------------------------------------------------
    COUNT_IN <= (COUNT + 1) WHEN (COUNT < MAX_VALUE) ELSE (others => '0'); 
    CNT : PROCESS(CLK, RST)
    BEGIN
        IF (RISING_EDGE(CLK)) THEN
            IF (RST ='1') THEN
                COUNT <= (others => '0');
            ELSIF (EN = '1') THEN
                COUNT <= COUNT_IN;
            END IF;
        END IF;
    END PROCESS;
    -------------------------------------------------------------------------------

    -- COUNTER OUTPUT -------------------------------------------------------------
    CNT_OUT <= STD_LOGIC_VECTOR(COUNT);
    -------------------------------------------------------------------------------
    
END Behavioral;







