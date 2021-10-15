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

entity double_bfu is
    Port (
        clk : in std_logic;
        d   : in double_bfu_in_type;
        q   : out double_bfu_out_type
    );
end double_bfu;

architecture Behavioral of double_bfu is

    signal bfu1d, bfu2d : butterfly_in_type;
    signal bfu1q, bfu2q : butterfly_out_type;
    signal bfsd : bfu_subtracter_in_type;
    signal bfsq : bfu_subtracter_out_type;
    signal omega_delayed : payload_array(0 to 1);
    
begin

bfu1: entity work.butterfly_dsp
port map(
    clk => clk,
    d => bfu1d,
    q => bfu1q
);

bfu2: entity work.butterfly_dsp
port map(
    clk => clk,
    d => bfu2d,
    q => bfu2q
);

subtracter: entity work.bfu_subtracter
port map (
    clk => clk,
    d => bfsd,
    q => bfsq
);

bfsd.en <= d.en and d.inv;
bfsd.a <= d.a;
bfsd.b <= d.b;

bfu1d.en <= d.en;
bfu2d.en <= d.en;
bfu1d.inv <= d.inv;
bfu2d.inv <= d.inv;

bfu1d.a <= d.a(0);
bfu2d.a <= d.a(1);
bfu1d.b <= d.b(0);
bfu2d.b <= d.b(1);
bfu1d.a_minus_b <= bfsq.sign(0) & bfsq.p(0);
bfu2d.a_minus_b <= bfsq.sign(1) & bfsq.p(1);

q.a(0) <= bfu1q.a;
q.b(0) <= bfu1q.b;
q.a(1) <= bfu2q.a;
q.b(1) <= bfu2q.b;

delay_omega: process(clk)
begin
    if rising_edge(clk)
    then
        if d.inv = '1'
        then
            omega_delayed <= d.omega;
        end if;
    end if;
end process;

mux: process(d.inv, d.omega, omega_delayed)
begin
    if d.inv = '0'
    then
        bfu1d.omega <= d.omega(0);
        bfu2d.omega <= d.omega(1);
    else
        bfu1d.omega <= omega_delayed(0);
        bfu2d.omega <= omega_delayed(1);
    end if;
end process;

end Behavioral;
