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

entity ntt_addr_gen is
    Port (
        clk : in std_logic;
        d : in ntt_addr_gen_in_type;
        q : out ntt_addr_gen_out_type
    );
end ntt_addr_gen;

architecture Behavioral of ntt_addr_gen is

signal counter : natural range 0 to 511;
signal stage : natural range 0 to 7;
signal counter_vec : std_logic_vector(8 downto 0);

type raw_addr_type is array(0 to 3) of std_logic_vector(15 downto 0);
signal raw_addr : raw_addr_type;
signal done_internal : std_logic;

signal omega_counter : natural range 0 to 255;

begin

mp: process(clk)
begin
    if rising_edge(clk)
    then
        if d.rst = '1'
        then
            counter <= 0;
            if d.inv = '0'
            then
                stage <= 0;
            else
                stage <= 7;
            end if;
            done_internal <= '0';
            omega_counter <= 0;
            
            q.ready <= '1';
        elsif d.en='1' and done_internal = '0'
        then
            q.ready <= '0';
            counter <= counter + 1;
            if stage = 7
            then
                omega_counter <= omega_counter + 2;
            elsif ((counter+1) mod (2**(6-stage))) = 0
            then
                omega_counter <= omega_counter + 1;
            end if;
            if ((counter+1) mod 64) = 0
            then
                if d.inv = '0'
                then
                    if stage /= 7
                    then
                        stage <= stage + 1;
                    else
                        done_internal <= '1';
                        q.ready <= '1';
                    end if;
                else -- inv = '1'
                    if stage /= 0
                    then
                        stage <= stage - 1;
                    else
                        done_internal <= '1';
                        q.ready <= '1';
                    end if;
                end if; -- inv '1'
            end if; -- counter = 63
        end if; -- en = '1' and done_internal='0'
    end if; -- clk
end process;

counter_vec <= std_logic_vector(to_unsigned(counter, counter_vec'length));

raw_addr_gen: for i in 0 to 3
generate
    raw_addr_gen2: for j in 0 to 5
    generate
        raw_addr(i)(j+2)   <= counter_vec(j);
        raw_addr(i)(j+2+8) <= counter_vec(j);
    end generate;
end generate;

raw_addr(0)(0) <= '0';
raw_addr(0)(8) <= '0';
raw_addr(0)(1) <= '0';
raw_addr(0)(9) <= '0';

raw_addr(1)(0) <= '1';
raw_addr(1)(8) <= '1';
raw_addr(1)(1) <= '0';
raw_addr(1)(9) <= '0';

raw_addr(2)(0) <= '0';
raw_addr(2)(8) <= '0';
raw_addr(2)(1) <= '1';
raw_addr(2)(9) <= '1';

raw_addr(3)(0) <= '1';
raw_addr(3)(8) <= '1';
raw_addr(3)(1) <= '1';
raw_addr(3)(9) <= '1';

agen: for i in 0 to 3
generate
    q.addr(i) <= raw_addr(i)(15-stage downto 8-stage);
end generate;

q.twiddlectrl.en(0) <= '1' when counter = 0 or stage = 7 or (counter mod (2**(6-stage))) = 0 else '0';
q.twiddlectrl.en(1) <= '1' when stage = 7 else '0';
q.twiddlectrl.addr(0)(7 downto 0) <= std_logic_vector(to_unsigned(omega_counter, 8));
q.twiddlectrl.addr(1)(7 downto 0) <= std_logic_vector(to_unsigned(omega_counter + 1, 8));
q.twiddlectrl.addr(0)(8) <= d.inv;
q.twiddlectrl.addr(1)(8) <= d.inv;

end Behavioral;
