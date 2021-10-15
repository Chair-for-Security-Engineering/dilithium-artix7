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
use work.dilithium.all;
use work.interfaces.all;
use work.memmap.all;

entity memory is
    Port (
        clk : in std_logic;
        d   : in memory_in_type;
        q   : out memory_out_type
    );
end memory;

architecture Behavioral of memory is
    
begin

-- generate memories
-- each one stores (up to) 8 polynomials in 4 18K BRAMs 
memgen: for i in 0 to NUM_MEM_8_POLY-1
generate
    mem: entity work.mem_8_poly
    port map (
        clk => clk,
        d => d(i),
        q => q(i)
    );
end generate;

end Behavioral;
