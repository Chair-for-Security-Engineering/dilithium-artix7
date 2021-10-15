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
use work.parmset.all;
use work.memmap.all;

package dilithium is

    type parmsets_type is record
        k : natural;
        l : natural;
        tau : natural;
        loggamma1 : natural;
        gamma2 : natural;
        highbits_len : natural;
        eta : natural;
        beta : natural;
        omega : natural;
    end record parmsets_type;
    type parmsets_array is array (2 to 4) of parmsets_type;
    
    constant PARMSETS : parmsets_array := ( 
    (k => 4, l => 4, tau => 39, loggamma1 => 17, gamma2 => 95232, highbits_len => 6, eta => 2, beta => 78, omega => 80),
    (k => 6, l => 5, tau => 49, loggamma1 => 19, gamma2 => 261888, highbits_len => 4, eta => 4, beta => 196, omega => 55),
    (k => 8, l => 7, tau => 60, loggamma1 => 19, gamma2 => 261888, highbits_len => 4, eta => 2, beta => 120, omega => 75)
    );

    constant DILITHIUM_N : integer := 256;
    constant DILITHIUM_LOG_N : integer := 8;
    constant DILITHIUM_Q : integer := 8380417;
    constant DILITHIUM_d : natural := 13;
    
    constant DILITHIUM_k : natural := PARMSETS(DILITHIUM_PARMSET).k;
    constant DILITHIUM_l : natural := PARMSETS(DILITHIUM_PARMSET).l;
    constant INACTIVE_k : natural := DILITHIUM_k;
    constant INACTIVE_l : natural := DILITHIUM_l;
    constant DILITHIUM_loggamma1 : natural := PARMSETS(DILITHIUM_PARMSET).loggamma1;
    constant DILITHIUM_beta : natural := PARMSETS(DILITHIUM_PARMSET).beta;
    constant DILITHIUM_gamma2 : natural := PARMSETS(DILITHIUM_PARMSET).gamma2;
    constant DILITHIUM_eta : natural := PARMSETS(DILITHIUM_PARMSET).eta;
    constant DILITHIUM_highbits_len : natural := PARMSETS(DILITHIUM_PARMSET).highbits_len;
    constant DILITHIUM_tau : natural := PARMSETS(DILITHIUM_PARMSET).tau;
    constant DILITHIUM_omega : natural := PARMSETS(DILITHIUM_PARMSET).omega;

    constant SHAKE256_RATE : integer := 1088;
    constant SHAKE128_RATE : integer := 1344;
    
    type payload_array is array(natural range <>) of std_logic_vector(22 downto 0);
    type coef_addr_array is array(natural range <>) of std_logic_vector(7 downto 0);

-------------
-- opcodes --
-------------
    constant PAYLOAD_TYPE_PK   : std_logic_vector(1 downto 0) := "00";
    constant PAYLOAD_TYPE_SK   : std_logic_vector(1 downto 0) := "01";
    constant PAYLOAD_TYPE_SIG  : std_logic_vector(1 downto 0) := "10";
    constant PAYLOAD_TYPE_SEED : std_logic_vector(1 downto 0) := "11";
    
    constant OPCODE_IDLE : std_logic_vector(3 downto 0)         := "0000";
    constant OPCODE_STOR : std_logic_vector(3 downto 0)         := "1100"; -- upper bit indicates 2-bit opcode with 2-bit parameter
    constant OPCODE_LOAD : std_logic_vector(3 downto 0)         := "1000";
    constant OPCODE_DIGEST_MSG : std_logic_vector(3 downto 0)   := "0001"; -- upper bit indicates 4-bit opcode
    constant OPCODE_SIGN : std_logic_vector(3 downto 0)         := "0010";
    constant OPCODE_SIGN_PRECOMP : std_logic_vector(3 downto 0) := "0011"; 
    constant OPCODE_VRFY : std_logic_vector(3 downto 0)         := "0100";
    constant OPCODE_VRFY_PRECOMP : std_logic_vector(3 downto 0) := "0101";
    constant OPCODE_KGEN : std_logic_vector(3 downto 0)         := "0111";
    
end dilithium;
