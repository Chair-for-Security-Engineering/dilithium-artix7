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
library UNISIM;
use UNISIM.VComponents.all;

library work;
use work.dilithium.all;
use work.interfaces.all;

entity reg32 is
    Generic (
        width : natural := 256
    );
    Port (
        clk : in std_logic;
        d   : in reg32_in_type;
        q   : out reg32_out_type
    );
end reg32;

architecture Behavioral of reg32 is

    signal data : std_logic_vector(width-1 downto 0);
    
    signal din : std_logic_vector(31 downto 0);
    
--    constant depth4 : std_logic_vector(3 downto 0) := std_logic_vector(to_unsigned(width / 32 - 1, 4));
--    constant depth5 : std_logic_vector(4 downto 0) := std_logic_vector(to_unsigned(width / 32 - 1, 5));

begin

q.data <= data(width-1 downto width-32);

reg: process(clk)
begin
    if rising_edge(clk)
    then
        if d.en_rotate = '1'
        then
            data(width-1 downto 32) <= data(width-32-1 downto 0);
            if d.en_write = '1'
            then
                data(31 downto 0) <= d.data;
            else
                data(31 downto 0) <= data(width-1 downto width-32);
            end if;
        end if;
    end if;
end process;

--din <= q.data when d.en_write = '0' else d.data;

--genLUT: for i in 0 to 31
--generate
--   gen16: if width/32 <= 16
--   generate
--      SRL16E_inst : SRL16E
--      generic map (
--         INIT => X"0000")
--      port map (
--         Q => q.data(i),       -- SRL data output
--         A0 => depth4(0),     -- Select[0] input
--         A1 => depth4(1),     -- Select[1] input
--         A2 => depth4(2),     -- Select[2] input
--         A3 => depth4(3),     -- Select[3] input
--         CE => d.en_rotate,     -- Clock enable input
--         CLK => CLK,   -- Clock input
--         D => din(i)        -- SRL data input
--      );
--   end generate;
--   gen32: if width/32 > 16 and width/32 <= 32
--   generate
--      SRLC32E_inst : SRLC32E
--      generic map (
--         INIT => X"00000000")
--      port map (
--         Q => q.data(i),        -- SRL data output
--         Q31 => open,    -- SRL cascade output pin
--         A => depth5,        -- 5-bit shift depth select input
--         CE => d.en_rotate,      -- Clock enable input
--         CLK => CLK,    -- Clock input
--         D => din(i)         -- SRL data input
--      );
--   end generate;
--end generate;

end Behavioral;
