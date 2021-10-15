-- COPYRIGHT (c) 2021 ALL RIGHT RESERVED
-- Chair for Security Engineering
-- Georg Land (georg.land@rub.de)
-- License: see LICENSE file

-- THIS CODE AND INFORMATION ARE PROVIDED "AS IS" WITHOUT WARRANTY OF ANY 
-- KIND, EITHER EXPRESSED OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE
-- IMPLIED WARRANTIES OF MERCHANTABILITY AND/OR FITNESS FOR A
-- PARTICULAR PURPOSE.

LIBRARY IEEE;
    USE IEEE.STD_LOGIC_1164.ALL;
    USE IEEE.NUMERIC_STD.ALL;
    
Library UNISIM;
use UNISIM.vcomponents.all;

library work;
use work.dilithium.all;
use work.interfaces.all;

entity red_dsp_2 is
    port (
        clk : in std_logic;
        d   : in red_dsp_2_in_type;
        q   : out red_dsp_2_out_type
    );
end red_dsp_2;

architecture Behavioral of red_dsp_2 is
    signal ALUMODE : std_logic_vector(3 downto 0);
    signal INMODE : std_logic_vector(4 downto 0);
    signal OPMODE : std_logic_vector(6 downto 0);
    signal A : std_logic_vector(29 downto 0);
    signal C : std_logic_vector(47 downto 0);
    signal DD : std_logic_vector(24 downto 0);
    signal P : std_logic_vector(47 downto 0);
    constant rst : std_logic := '0';
begin

    -- DSP input signals
    A(24 downto 2) <= (others => '0');
    A(1 downto 0) <= d.a; 
    C(47 downto 24) <= (others => '0');
    DD(24 downto 23) <= "00";
    DD(22 downto 0) <= d.d;
              
    -- delay C
    delay_c: entity work.dyn_shift_reg
    generic map (width => 24, max_depth => 1)
    port map (
        clk => clk,
        ce => d.en,
        depth => 1,
        d => d.c,
        q => C(23 downto 0)
    );
    
    -- output
    q.p <= P(26 downto 0);
    
    -- choose ALUMODE, INMODE and OPMODE according to sel_delayed
    OPMODE <= "0110101"; -- input Z=C and XY=M
    INMODE(4) <= '0'; -- don't care
    INMODE(3 downto 0) <= "1100"; -- M=D-A
    ALUMODE <= "0001"; -- -Z+(X+Y+CIN)-1
                

   -- DSP48E1: 48-bit Multi-Functional Arithmetic Block
   --          Artix-7
   -- Xilinx HDL Language Template, version 2020.2

   DSP48E1_inst : DSP48E1
   generic map (
      -- Feature Control Attributes: Data Path Selection
      A_INPUT => "DIRECT",               -- Selects A input source, "DIRECT" (A port) or "CASCADE" (ACIN port)
      B_INPUT => "DIRECT",               -- Selects B input source, "DIRECT" (B port) or "CASCADE" (BCIN port)
      USE_DPORT => TRUE,                -- Select D port usage (TRUE or FALSE)
      USE_MULT => "MULTIPLY",            -- Select multiplier usage ("MULTIPLY", "DYNAMIC", or "NONE")
      USE_SIMD => "ONE48",               -- SIMD selection ("ONE48", "TWO24", "FOUR12")
      -- Pattern Detector Attributes: Pattern Detection Configuration
      AUTORESET_PATDET => "NO_RESET",    -- "NO_RESET", "RESET_MATCH", "RESET_NOT_MATCH" 
      MASK => X"3fffffffffff",           -- 48-bit mask value for pattern detect (1=ignore)
      PATTERN => X"000000000000",        -- 48-bit pattern match for pattern detect
      SEL_MASK => "MASK",                -- "C", "MASK", "ROUNDING_MODE1", "ROUNDING_MODE2" 
      SEL_PATTERN => "PATTERN",          -- Select pattern value ("PATTERN" or "C")
      USE_PATTERN_DETECT => "NO_PATDET", -- Enable pattern detect ("PATDET" or "NO_PATDET")
      -- Register Control Attributes: Pipeline Register Configuration
      ACASCREG => 0,                     -- Number of pipeline stages between A/ACIN and ACOUT (0, 1 or 2)
      ADREG => 1,                        -- Number of pipeline stages for pre-adder (0 or 1)
      ALUMODEREG => 1,                   -- Number of pipeline stages for ALUMODE (0 or 1)
      AREG => 0,                         -- Number of pipeline stages for A (0, 1 or 2)
      BCASCREG => 0,                     -- Number of pipeline stages between B/BCIN and BCOUT (0, 1 or 2)
      BREG => 0,                         -- Number of pipeline stages for B (0, 1 or 2)
      CARRYINREG => 0,                   -- Number of pipeline stages for CARRYIN (0 or 1)
      CARRYINSELREG => 0,                -- Number of pipeline stages for CARRYINSEL (0 or 1)
      CREG => 1,                         -- Number of pipeline stages for C (0 or 1)
      DREG => 0,                         -- Number of pipeline stages for D (0 or 1)
      INMODEREG => 0,                    -- Number of pipeline stages for INMODE (0 or 1)
      MREG => 1,                         -- Number of multiplier pipeline stages (0 or 1)
      OPMODEREG => 0,                    -- Number of pipeline stages for OPMODE (0 or 1)
      PREG => 1                          -- Number of pipeline stages for P (0 or 1)
   )
   port map (
      -- Cascade: 30-bit (each) output: Cascade Ports
      ACOUT => open,                   -- 30-bit output: A port cascade output
      BCOUT => open,                   -- 18-bit output: B port cascade output
      CARRYCASCOUT => open,     -- 1-bit output: Cascade carry output
      MULTSIGNOUT => open,       -- 1-bit output: Multiplier sign cascade output
      PCOUT => q.pcout,                   -- 48-bit output: Cascade output
      -- Control: 1-bit (each) output: Control Inputs/Status Bits
      OVERFLOW => open,             -- 1-bit output: Overflow in add/acc output
      PATTERNBDETECT => open, -- 1-bit output: Pattern bar detect output
      PATTERNDETECT => open,   -- 1-bit output: Pattern detect output
      UNDERFLOW => open,           -- 1-bit output: Underflow in add/acc output
      -- Data: 4-bit (each) output: Data Ports
      CARRYOUT => open,             -- 4-bit output: Carry output
      P => P,                           -- 48-bit output: Primary data output
      -- Cascade: 30-bit (each) input: Cascade Ports
      ACIN => (others => '0'),                     -- 30-bit input: A cascade data input
      BCIN => (others => '0'),                     -- 18-bit input: B cascade input
      CARRYCASCIN => '0',       -- 1-bit input: Cascade carry input
      MULTSIGNIN => '0',         -- 1-bit input: Multiplier sign input
      PCIN => (others => '0'),                     -- 48-bit input: P cascade input
      -- Control: 4-bit (each) input: Control Inputs/Status Bits
      ALUMODE => ALUMODE,               -- 4-bit input: ALU control input
      CARRYINSEL => (others => '0'),         -- 3-bit input: Carry select input
      CLK => CLK,                       -- 1-bit input: Clock input
      INMODE => INMODE,                 -- 5-bit input: INMODE control input
      OPMODE => OPMODE,                 -- 7-bit input: Operation mode input
      -- Data: 30-bit (each) input: Data Ports
      A => A,                           -- 30-bit input: A data input
      B => std_logic_vector(to_signed(1, 18)),                           -- 18-bit input: B data input
      C => C,                           -- 48-bit input: C data input
      CARRYIN => '1',               -- 1-bit input: Carry input signal
      D => DD,                           -- 25-bit input: D data input
      -- Reset/Clock Enable: 1-bit (each) input: Reset/Clock Enable Inputs
      CEA1 => d.en,                     -- 1-bit input: Clock enable input for 1st stage AREG
      CEA2 => d.en,                     -- 1-bit input: Clock enable input for 2nd stage AREG
      CEAD => d.en,                     -- 1-bit input: Clock enable input for ADREG
      CEALUMODE => d.en,           -- 1-bit input: Clock enable input for ALUMODE
      CEB1 => d.en,                     -- 1-bit input: Clock enable input for 1st stage BREG
      CEB2 => d.en,                     -- 1-bit input: Clock enable input for 2nd stage BREG
      CEC => d.en,                       -- 1-bit input: Clock enable input for CREG
      CECARRYIN => d.en,           -- 1-bit input: Clock enable input for CARRYINREG
      CECTRL => d.en,                 -- 1-bit input: Clock enable input for OPMODEREG and CARRYINSELREG
      CED => d.en,                       -- 1-bit input: Clock enable input for DREG
      CEINMODE => d.en,             -- 1-bit input: Clock enable input for INMODEREG
      CEM => d.en,                       -- 1-bit input: Clock enable input for MREG
      CEP => d.en,                       -- 1-bit input: Clock enable input for PREG
      RSTA => rst,                     -- 1-bit input: Reset input for AREG
      RSTALLCARRYIN => rst,   -- 1-bit input: Reset input for CARRYINREG
      RSTALUMODE => rst,         -- 1-bit input: Reset input for ALUMODEREG
      RSTB => rst,                     -- 1-bit input: Reset input for BREG
      RSTC => rst,                     -- 1-bit input: Reset input for CREG
      RSTCTRL => rst,               -- 1-bit input: Reset input for OPMODEREG and CARRYINSELREG
      RSTD => rst,                     -- 1-bit input: Reset input for DREG and ADREG
      RSTINMODE => rst,           -- 1-bit input: Reset input for INMODEREG
      RSTM => rst,                     -- 1-bit input: Reset input for MREG
      RSTP => rst                      -- 1-bit input: Reset input for PREG
   );

   -- End of DSP48E1_inst instantiation
				
end Behavioral;