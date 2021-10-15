-- COPYRIGHT (c) 2021 ALL RIGHT RESERVED
-- Chair for Security Engineering
-- Jan Richter-Brockmann, Georg Land (georg.land@rub.de)
-- License: see LICENSE file

-- THIS CODE AND INFORMATION ARE PROVIDED "AS IS" WITHOUT WARRANTY OF ANY 
-- KIND, EITHER EXPRESSED OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE
-- IMPLIED WARRANTIES OF MERCHANTABILITY AND/OR FITNESS FOR A
-- PARTICULAR PURPOSE.

LIBRARY IEEE;
    USE IEEE.STD_LOGIC_1164.ALL;
    USE IEEE.NUMERIC_STD.ALL;


PACKAGE keccak_settings IS 

    -- SETTINGS ------------------------------------------------------------------
    -- Settings KECCAK
    CONSTANT STATE_WIDTH        : integer := 1600;                  -- Size of the state (unrolled matrix)
    CONSTANT RATE               : integer := 1344;                  -- The rate is used to determine the number of random output bits
    CONSTANT N_R                : integer := 24;                    -- n_r = 12 + 2*log2(LANE_WIDTH) - How to calculate this in VHDL?
    
    CONSTANT ROTATE_WIDTH       : integer := 32;                    -- rate must be divisible by this
    
    -- Settings Counter
    CONSTANT CNT_LENGTH_ROUND   : integer := 5;                     -- ceil(CNT_LENGTH_ROUND = log_2(N_R))
    CONSTANT CNT_LENGTH_ROTATE  : integer := 6;
    
    -- CONSTANTS AND DEFINITIONS -------------------------------------------------
    -- This settings are calculated by the above ones
    CONSTANT LANE_WIDTH         : integer := state_width / 25;      -- LANE_WIDTH = w
    CONSTANT RATE_LANES         : integer := RATE / LANE_WIDTH; 
    CONSTANT NUM_ROTATE         : integer := RATE / ROTATE_WIDTH;

    -- define the keccak matrix
    SUBTYPE dim1 IS STD_LOGIC_VECTOR(LANE_WIDTH-1 DOWNTO 0);
    TYPE dim1_vector IS ARRAY(natural RANGE <>) OF dim1;
    SUBTYPE dim2 IS dim1_vector(4 DOWNTO 0);
    TYPE dim2_vector IS ARRAY(natural RANGE <>) OF dim2;
    SUBTYPE keccak_m IS dim2_vector(4 DOWNTO 0);
 
    -- define the index LUT for the roh step   
    TYPE indexlut IS ARRAY (0 TO 4, 0 TO 4) of integer RANGE 0 TO 63;
    CONSTANT rohindex : indexlut := ((0, 1, 62, 28, 27), (36, 44, 6, 55, 20), (3, 10, 43, 25, 39), (41, 45, 15, 21, 8), (18, 2, 61, 56, 14));

END PACKAGE;
