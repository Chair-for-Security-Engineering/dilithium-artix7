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

entity mem_8_poly is
    Port (
        clk : in std_logic;
        d   : in mem_8_poly_in_type;
        q   : out payload_array(0 to 3)
    );
end mem_8_poly;

architecture Behavioral of mem_8_poly is

    constant MEMORY_DELAY : natural := 3;
    
    type bankaddr_type is array(0 to 1, 0 to 3) of natural range 0 to 3;
    signal bankaddr, bankaddr_delay : bankaddr_type;
    type pipeline_type is array(1 to MEMORY_DELAY) of bankaddr_type;
    signal pipeline : pipeline_type;
    
    type memin_array is array(0 to 3) of mem_8_quarter_poly_in_type;
    signal dm, dm_reg : memin_array;
    signal qm : payload_array(0 to 3);
    
begin

dmreg: process(clk)
begin
    if rising_edge(clk)
    then
        dm_reg <= dm;
    end if;
end process;

memgen: for i in 0 to 3
generate
    mem: entity work.mem_8_quarter_poly
    port map (
        clk => clk,
        
        d => dm_reg(i),
        q => qm(i)
    );
end generate;

mux: process(bankaddr, bankaddr_delay, d, qm)
begin 
    for bank in 0 to 3
    loop
        dm(bank).raddr(8 downto 6) <= std_logic_vector(to_unsigned(d.rsel, 3));
        dm(bank).raddr(5 downto 0) <= (others => '0');
        dm(bank).waddr(8 downto 6) <= std_logic_vector(to_unsigned(d.wsel, 3));
        dm(bank).waddr(5 downto 0) <= (others => '0');
        dm(bank).ren <= '0';
        dm(bank).wen <= '0';
        dm(bank).data <= (others => '0');
        q(bank) <= (others => '0');
        for b in 0 to 3
        loop
            if bankaddr(0,b) = bank
            then
                dm(bank).raddr(5 downto 0) <= d.raddr(b)(7 downto 2);
                dm(bank).ren <= d.ren(b);
            end if;
            if bankaddr(1,b) = bank
            then
                dm(bank).waddr(5 downto 0) <= d.waddr(b)(7 downto 2);
                dm(bank).wen <= d.wen(b);
                dm(bank).data <= d.wdata(b);
            end if;
            if bankaddr_delay(0, bank) = b
            then
                q(bank) <= qm(b);
            end if;
        end loop; -- b in 0 to 3
    end loop; -- bank in 0 to 3
end process; -- mux

mem_bank_gen: for b in 0 to 3
generate
    bankaddr(0,b) <= (to_integer(unsigned(d.raddr(b)(1 downto 0))) 
                    + to_integer(unsigned(d.raddr(b)(3 downto 2))) 
                    + to_integer(unsigned(d.raddr(b)(5 downto 4))) 
                    + to_integer(unsigned(d.raddr(b)(7 downto 6)))) mod 4;
    bankaddr(1,b) <= (to_integer(unsigned(d.waddr(b)(1 downto 0))) 
                    + to_integer(unsigned(d.waddr(b)(3 downto 2))) 
                    + to_integer(unsigned(d.waddr(b)(5 downto 4))) 
                    + to_integer(unsigned(d.waddr(b)(7 downto 6)))) mod 4;
end generate;

assert MEMORY_DELAY >= 1 report "bad memory delay" severity failure;
bankaddr_delay <= pipeline(MEMORY_DELAY);
delay_bankaddr: process(clk)
begin
    if rising_edge(clk)
    then
        for i in MEMORY_DELAY downto 2
        loop
            pipeline(i) <= pipeline(i-1);
        end loop;
        pipeline(1) <= bankaddr;
    end if;
end process;

end Behavioral;
