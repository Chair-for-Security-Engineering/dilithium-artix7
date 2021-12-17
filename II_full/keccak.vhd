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
-- AUTHOR:					Jan Richter-Brockmann, Georg Land
--
-- CREATE DATE:			    13/12/2018
-- LAST CHANGES:            10/01/2020
-- MODULE NAME:			    KECCAK
--
-- REVISION:				1.00 - KECCAK top level
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
    USE IEEE.STD_LOGIC_UNSIGNED.ALL;
    USE IEEE.NUMERIC_STD.ALL;

LIBRARY work;
    USE work.keccak_settings.ALL;
    USE work.dilithium.ALL;
    use work.interfaces.ALL;
    

ENTITY KECCAK IS
    PORT ( CLK          : IN  STD_LOGIC;
           d            : IN  KECCAK_IN_TYPE;
           q            : OUT KECCAK_OUT_TYPE
           );
END KECCAK;

ARCHITECTURE Structural OF KECCAK IS



-- SIGNALS -----------------------------------------------------------------------
SIGNAL STATE_OUT                            : keccak_m := (OTHERS => (OTHERS => (OTHERS => '0')));
SIGNAL STATE_REG, STATE_REG_IN              : keccak_m;
SIGNAL STATE_REG_IN_ALIAS, STATE_REG_ALIAS  : STD_LOGIC_VECTOR(STATE_WIDTH-1 DOWNTO 0);
SIGNAL ROUND_NUMBER                         : STD_LOGIC_VECTOR(4 DOWNTO 0) := (OTHERS => '0');
SIGNAL ROUND_NUMBER_IN                      : STD_LOGIC_VECTOR(4 DOWNTO 0) := (OTHERS => '0');
SIGNAL DATA_OUT_TMP                         : STD_LOGIC_VECTOR (RATE-1 DOWNTO 0);

-- COUNTER
SIGNAL CNT_RST_ROUND                        : STD_LOGIC;
SIGNAL CNT_ROUND                            : STD_LOGIC_VECTOR((CNT_LENGTH_ROUND-1) DOWNTO 0);
SIGNAL CNT_RST_ROTATE                       : STD_LOGIC;
SIGNAL CNT_ROTATE                           : STD_LOGIC_VECTOR((CNT_LENGTH_ROTATE-1) DOWNTO 0);

SIGNAL RESET_IO                             : STD_LOGIC;
SIGNAL ENABLE_ROUND                         : STD_LOGIC;
SIGNAL ENABLE_IO                            : STD_LOGIC;



-- STRUCTURAL ---------------------------------------------------------------------
BEGIN

    -- I/O Register  
    keccak_reg : PROCESS (clk, RESET_IO)
    BEGIN
        IF(RISING_EDGE(clk)) THEN
            IF(RESET_IO ='1') THEN
                STATE_REG <= (OTHERS => (OTHERS => (OTHERS => '0')));
            ELSE
                IF ENABLE_ROUND = '1' THEN
                    STATE_REG <= STATE_OUT;
                ELSIF ENABLE_IO = '1' THEN
                    STATE_REG <= STATE_REG_IN;
                END IF;
            END IF;
        END IF;
    END PROCESS;
    
    STATE_REG_IN_ALIAS(STATE_WIDTH-RATE+ROTATE_WIDTH-1 DOWNTO STATE_WIDTH-RATE) <= d.data;
    STATE_REG_IN_ALIAS(STATE_WIDTH-1 DOWNTO STATE_WIDTH-RATE+ROTATE_WIDTH) <= STATE_REG_ALIAS(STATE_WIDTH-ROTATE_WIDTH-1 DOWNTO STATE_WIDTH-RATE);
    STATE_REG_IN_ALIAS(STATE_WIDTH-RATE-1 DOWNTO 0) <= STATE_REG_ALIAS(STATE_WIDTH-RATE-1 DOWNTO 0);
    q.data <= STATE_REG_ALIAS(STATE_WIDTH-1 DOWNTO STATE_WIDTH-ROTATE_WIDTH);
    
--    STATE_REG_IN_ALIAS(STATE_WIDTH-1 DOWNTO STATE_WIDTH-RATE) <= DATA_IN;
--    STATE_REG_IN_ALIAS(STATE_WIDTH-RATE-1 DOWNTO 0) <= STATE_REG_ALIAS(STATE_WIDTH-RATE-1 DOWNTO 0);
--    DATA_OUT <= STATE_REG_ALIAS(STATE_WIDTH-1 DOWNTO STATE_WIDTH-RATE);
    
    a001 : FOR i IN 0 to 24
    GENERATE
        a002 : FOR j in 0 to 7
        GENERATE
            STATE_REG_ALIAS(STATE_WIDTH - i * LANE_WIDTH - j*8 - 1 DOWNTO STATE_WIDTH - i * LANE_WIDTH - j*8 - 8) <= STATE_REG(i mod 5)(i / 5)(j*8+7 downto j*8);
            STATE_REG_IN(i mod 5)(i / 5)(j*8+7 downto j*8) <= STATE_REG_IN_ALIAS(STATE_WIDTH - i * LANE_WIDTH - j*8 - 1 DOWNTO STATE_WIDTH - i * LANE_WIDTH - j*8 - 8);
        END GENERATE;
    END GENERATE a001;
                    
    -- KECCAK round function
    KECCAK_ROUND : ENTITY work.keccak_round
        PORT MAP (
            STATE_IN     => STATE_REG,
            STATE_OUT    => STATE_OUT,
            ROUND_NUMBER => CNT_ROUND
        );
    
    -- Round Counter
    COUNTER_ROUND : ENTITY work.KECCAK_COUNTER
    GENERIC MAP (
        SIZE            => CNT_LENGTH_ROUND,
        MAX_VALUE       => N_R-1)
    PORT MAP (
        CLK             => CLK,
        EN              => ENABLE_ROUND,
        RST             => CNT_RST_ROUND,
        CNT_OUT         => CNT_ROUND
    );
    
    -- rotation counter
    COUNTER_ROTATE : ENTITY work.KECCAK_COUNTER
    GENERIC MAP (
        SIZE            => CNT_LENGTH_ROTATE,
        MAX_VALUE       => NUM_ROTATE-1)
    PORT MAP (
        CLK             => CLK,
        EN              => ENABLE_IO,
        RST             => CNT_RST_ROTATE,
        CNT_OUT         => CNT_ROTATE
    );
    
    -- KECCAK Finite State Machine
    FSM : ENTITY work.KECCAK_CONTROLLER
    PORT MAP (
        CLK             => CLK,
        RESET           => d.rst,
        START           => d.en,
        READY           => q.ready,
        ENABLE_ROUND    => ENABLE_ROUND,
        RESET_IO        => RESET_IO,
        ENABLE_IO       => ENABLE_IO,
        CNT_RST_ROUND   => CNT_RST_ROUND,
        CNT_ROUND       => CNT_ROUND,
        CNT_RST_ROTATE  => CNT_RST_ROTATE,
        CNT_ROTATE      => CNT_ROTATE
    );

END Structural;
