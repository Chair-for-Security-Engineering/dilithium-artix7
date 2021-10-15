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

entity fifo is
    Generic (
        input_length : natural := 32;
        output_length : natural := 40;
        buf_width : natural := 160 -- should be lcm of both above
    );
    Port (
        clk : in std_logic;
        d   : in fifo_in_type;
        datad : in std_logic_vector(input_length-1 downto 0);
        q   : out fifo_out_type;
        dataq : out std_logic_vector(output_length-1 downto 0)
    );
end fifo;

architecture Behavioral of fifo is

    type state_type is (idle, load, store);
    signal state, nextstate : state_type;

    signal bufreg : std_logic_vector(buf_width-1 downto 0);
    signal bufreg_en_load, bufreg_en_store : std_logic;
    
    signal lcntd, scntd : counter_in_type;
    signal lcntq, scntq : counter_out_type;

begin

buffer_register: process(clk)
begin
    if rising_edge(clk)
    then
        if bufreg_en_load = '1'
        then
            for i in 0 to input_length/8-1
            loop
                bufreg(bufreg'high-i*8 downto bufreg'length-i*8-8) <= datad(i*8+7 downto i*8);
            end loop;
            bufreg(bufreg'high-input_length downto 0) <= bufreg(bufreg'high downto input_length);
        elsif bufreg_en_store = '1'
        then
            bufreg(bufreg'high-output_length downto 0) <= bufreg(bufreg'high downto output_length);
            bufreg(bufreg'high downto bufreg'length-output_length) <= (others => '0');
        end if;
    end if;
end process;
dataq <= bufreg(output_length-1 downto 0);

load_counter: entity work.counter
generic map (max_value => buf_width/input_length-1)
port map (
    clk => clk,
    d => lcntd,
    q => lcntq,
    value => open
);

store_counter: entity work.counter
generic map (max_value => buf_width/output_length-1)
port map (
    clk => clk,
    d => scntd,
    q => scntq,
    value => open
);

states: process(clk)
begin
    if rising_edge(clk)
    then
        if d.rst = '1'
        then
            state <= idle;
        else
            state <= nextstate;
        end if;
    end if;
end process;

signals: process(state, d, lcntq, scntq)
begin
    nextstate <= state;
    
    q.ready <= '0';
    q.ready_rcv <= '0';
    q.valid <= '0';
    q.toggle <= '0';
    
    lcntd.en <= '0';
    lcntd.rst <= '0';
    scntd.en <= '0';
    scntd.rst <= '0';
    
    bufreg_en_store <= '0';
    bufreg_en_load <= '0';
    
    case state is
        when idle =>
            q.ready <= '1';
            
            lcntd.rst <= '1';
            scntd.rst <= '1';
            if d.en = '1'
            then
                nextstate <= load;
            end if;
        
        when load =>
            q.ready_rcv <= '1';
            
            lcntd.en <= d.valid;
            bufreg_en_load <= d.valid;
            scntd.rst <= '1';
            
            if lcntq.max = '1' and d.valid = '1'
            then
                q.toggle <= '1';
                nextstate <= store;
            end if;
            
        when store =>
            q.valid <= '1';
        
            scntd.en <= d.ready_rcv;
            bufreg_en_store <= d.ready_rcv;
            lcntd.rst <= '1';
            
            if scntq.max = '1' and d.ready_rcv = '1'
            then
                q.toggle <= '1';
                nextstate <= load;
            end if;
    end case;
end process;


end Behavioral;
