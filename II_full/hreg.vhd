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

entity hreg is
    Port (
        clk : in std_logic;
        d   : in hreg_in_type;
        q   : out hreg_out_type
    );
end hreg;

architecture Behavioral of hreg is

    type byte_array is array(natural range <>) of std_logic_vector(7 downto 0);
    signal data_off : byte_array(0 to DILITHIUM_omega-1);
    signal poly_off : byte_array(0 to DILITHIUM_k-1);

begin

q.data_offset <= data_off(0);
q.poly_offset <= poly_off(0);

r: process(clk)
begin
    if rising_edge(clk)
    then
        if d.en_rotate_data = '1'
        then
            data_off(0 to DILITHIUM_omega-2) <= data_off(1 to DILITHIUM_omega-1);
            data_off(DILITHIUM_omega-1) <= data_off(0);
            if d.en_write_data = '1'
            then
                data_off(DILITHIUM_omega-1) <= d.data_offset;
            end if;
        end if;
        if d.en_rotate_poly = '1'
        then
            poly_off(0 to DILITHIUM_k-2) <= poly_off(1 to DILITHIUM_k-1);
            poly_off(DILITHIUM_k-1) <= poly_off(0);
            if d.en_write_poly = '1'
            then
                poly_off(DILITHIUM_k-1) <= d.poly_offset;
            end if;
        end if;
    end if;
end process;

end Behavioral;
