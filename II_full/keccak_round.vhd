----------------------------------------------------------------------------------
-- COPYRIGHT (c) 2018 ALL RIGHT RESERVED
--
-- COMPANY:					Ruhr-University Bochum, Chair for Security Engineering
-- AUTHOR:					Jan Richter-Brockmann
--
-- CREATE DATE:			    13/12/2018
-- LAST CHANGES:            13/12/2018
-- MODULE NAME:			    KECCAK_ROUND
--
-- REVISION:				1.00 - Implements the KECCAK permutation function
--
-- LICENCE: 				Please look at licence.txt
-- USAGE INFORMATION:	    Please look at readme.txt. If licence.txt or readme.txt
--							are missing or if you have questions regarding the code
--							please contact Tim Güneysu (tim.gueneysu@rub.de) and
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
    
    

ENTITY KECCAK_ROUND IS
    PORT ( state_IN     : IN  keccak_m;
           state_out    : OUT keccak_m;
           round_number : IN  STD_LOGIC_VECTOR ((CNT_LENGTH_ROUND-1) DOWNTO 0)
           );
END KECCAK_ROUND;


ARCHITECTURE Behavioral OF KECCAK_ROUND IS



-- SIGNALS -----------------------------------------------------------------------
SIGNAL theta_in, theta_out, roh_in, roh_out, pi_in, pi_out, chi_in, chi_out, iota_in, iota_out  : keccak_m  := (OTHERS => (OTHERS => (OTHERS => '0')));
SIGNAL theta_int1, theta_int2                                                                   : dim2      := (OTHERS => (OTHERS => '0'));
SIGNAL keccak_roundconst                                                                        : STD_LOGIC_VECTOR (LANE_WIDTH-1 DOWNTO 0);



-- BEHAVIORAL --------------------------------------------------------------------
BEGIN

    theta_in <= state_in;
    
    -- Theta step
    t001: FOR x IN 0 TO 4 GENERATE
        t002: FOR z IN 0 TO LANE_WIDTH-1 GENERATE
            theta_int1(x)(z) <= theta_in(x)(0)(z) XOR theta_in(x)(1)(z) XOR theta_in(x)(2)(z) XOR theta_in(x)(3)(z) XOR theta_in(x)(4)(z);
        END GENERATE;
    END GENERATE;
    
    t003: FOR x IN 0 TO 4 GENERATE
        t004: FOR z IN 0 TO LANE_WIDTH-1 GENERATE
            theta_int2(x)(z) <= theta_int1((x-1) MOD 5)(z) XOR theta_int1((x+1) MOD 5)((z-1) MOD LANE_WIDTH);
        END GENERATE;
    END GENERATE;
    
    t005: FOR x IN 0 TO 4 GENERATE
        t006: FOR y IN 0 TO 4 GENERATE
            t007: FOR z IN 0 TO LANE_WIDTH-1 GENERATE
                theta_out(x)(y)(z) <= theta_in(x)(y)(z) XOR theta_int2(x)(z);
            END GENERATE;
        END GENERATE;
    END GENERATE;

    roh_IN <= theta_out;
    
    -- Roh step
    r001: FOR x IN 0 TO 4 GENERATE
        r002: FOR y IN 0 TO 4 GENERATE
            r003: FOR z IN 0 TO LANE_WIDTH-1 GENERATE
                roh_out(x)(y)(z) <= roh_in(x)(y)((z-rohindex(y, x)) MOD LANE_WIDTH);
            END GENERATE;
        END GENERATE;
    END GENERATE;
    
    pi_in <= roh_out;
    
    -- Pi step
    p001: FOR x IN 0 TO 4 GENERATE
        p002: FOR y IN 0 TO 4 GENERATE
            p003: FOR z IN 0 TO LANE_WIDTH-1 GENERATE
                pi_out(y)((2*x+3*y) MOD 5)(z) <= pi_in(x)(y)(z);
            END GENERATE;
        END GENERATE;
    END GENERATE;

    chi_in <= pi_out;
    
    -- Chi step
    c001: FOR x IN 0 TO 4 GENERATE
        c002: FOR y IN 0 TO 4 GENERATE
            c003: FOR z IN 0 TO LANE_WIDTH-1 GENERATE
                chi_out(x)(y)(z) <= chi_in(x)(y)(z) XOR ((NOT chi_in((x+1) MOD 5)(y)(z)) AND chi_in((x+2) MOD 5)(y)(z));
            END GENERATE;
        END GENERATE;
    END GENERATE;     
    
    iota_in <= chi_out;
    
    -- Iota step
    i001: FOR x IN 0 TO 4 GENERATE
        i002 : FOR y IN 0 TO 4 GENERATE
            i003 : FOR z IN 0 TO LANE_WIDTH-1 GENERATE
                i004 : IF x = 0 AND y =0 GENERATE
                    iota_out(0)(0)(z) <= iota_in(0)(0)(z) XOR keccak_roundconst(z);
                END GENERATE;
                i005 : IF x /= 0 or y /= 0 GENERATE
                    iota_out(x)(y)(z) <= iota_in(x)(y)(z);
                END GENERATE;
            END GENERATE;
        END GENERATE;
    END GENERATE;

    state_out <= iota_out;

    -- Initiate the RCs
    KECCAK_RC : ENTITY WORK.KECCAK_RC
        PORT MAP (
            round     => round_number,
            const_out => keccak_roundconst        
        );  

END Behavioral;
