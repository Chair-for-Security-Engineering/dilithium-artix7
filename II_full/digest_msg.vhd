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

entity digest_msg is
Port (
    clk : in std_logic;
    d   : in digest_msg_in_type;
    q   : out digest_msg_out_type
);
end digest_msg;

architecture Behavioral of digest_msg is

    type state_type is (idle, absorb_tr, rcv, absorb, zeropad, zeropad_squeeze, permute, padding_start, padding_finish, permute_squeeze, squeeze, reset);
    signal state, nextstate : state_type;

    signal shacntd : counter_in_type;
    signal shacntq : counter_out_type;
    signal shacnt : natural range 0 to SHAKE256_RATE/32-1;
    
    signal buf : std_logic_vector(31 downto 0);
    signal buf_en : std_logic := '0';
    signal buf_i : natural range 0 to 3 := 0;

begin

q.muregd.data <= d.keccakq.data;

keccakin: process(state, d, shacntq, buf_i)
begin
    q.keccakd.data <= d.keccakq.data;
    case state is
        when absorb_tr => q.keccakd.data <= d.trregq.data;
        when absorb    => q.keccakd.data <= d.payload xor d.keccakq.data;
        when padding_start =>
            if shacntq.max = '1'
            then
                case buf_i is
                    when 0 => q.keccakd.data <= x"1f000080" xor d.keccakq.data;
                    when 1 => q.keccakd.data <= buf(31 downto 24) & x"1f0080" xor d.keccakq.data;
                    when 2 => q.keccakd.data <= buf(31 downto 16) & x"1f80" xor d.keccakq.data;
                    when others => q.keccakd.data <= buf(31 downto 8) & x"9f" xor d.keccakq.data;
                end case;
            else
                case buf_i is
                    when 0 => q.keccakd.data <= x"1f000000" xor d.keccakq.data;
                    when 1 => q.keccakd.data <= buf(31 downto 24) & x"1f0000" xor d.keccakq.data;
                    when 2 => q.keccakd.data <= buf(31 downto 16) & x"1f00" xor d.keccakq.data;
                    when others => q.keccakd.data <= buf(31 downto 8) & x"1f" xor d.keccakq.data;
                end case;
            end if;
            
        when padding_finish =>
            if shacntq.max = '1'
            then
                q.keccakd.data <= x"00000080" xor d.keccakq.data;
            else
                q.keccakd.data <= d.keccakq.data;
            end if;
            
        when others => q.keccakd.data <= d.keccakq.data;
    end case;
end process;

inbuffer: process(clk)
begin
    if rising_edge(clk)
    then
        if buf_en = '1'
        then
            buf(31-buf_i*8 downto 32-(buf_i+1)*8) <= d.payload(7 downto 0);
            buf_i <= (buf_i + 1) mod 4;
        end if;
    end if;
end process;

sha_counter: entity work.counter
generic map (max_value => SHAKE256_RATE/8-1)
port map (
    clk => clk,
    d => shacntd,
    q => shacntq,
    value => shacnt
);

states: process(clk)
begin
    if rising_edge(clk)
    then
        state <= nextstate;
    end if;
end process;

signals: process(state, d, shacntq, shacnt, buf_i)
begin

    nextstate <= state;
    
    q.ready <= '0';
    q.ready_rcv <= '0';
    
    q.keccakd.en <= '0';
    q.keccakd.rst <= '0';
    
    buf_en <= '0';
    
    q.trregd.en_rotate <= '0';
    q.trregd.en_write <= '0';
    q.muregd.en_rotate <= '0';
    q.muregd.en_write <= '0';
    
    shacntd.en <= '0';
    shacntd.rst <= '0';
    
    case state is
        when idle =>
            q.ready <= '1';
            q.keccakd.rst <= '0';
            shacntd.rst <= '1';
            
            if d.en = '1'
            then
                q.keccakd.en <= '1';
                q.keccakd.rst <= '0';
                nextstate <= absorb_tr;
            end if;
            
        when absorb_tr =>
            q.keccakd.en <= '1';
            shacntd.en <= '1';
            q.trregd.en_rotate <= '1';
            
            if shacnt = 256/32-1
            then
                nextstate <= rcv;
            end if;
          
        when rcv =>
            q.ready_rcv <= '1';
            buf_en <= d.valid;
            if d.valid = '1' and buf_i = 3
            then
                nextstate <= absorb;
            elsif d.done = '1'
            then
                nextstate <= padding_start;
            end if;
            
        when absorb =>
            q.ready_rcv <= '0';
            q.keccakd.en <= '1';
            shacntd.en <= '1';
            
            if shacntq.max = '1' and d.valid = '1'
            then
                q.ready_rcv <= '0';
                nextstate <= zeropad;
            elsif d.done = '1'
            then
                nextstate <= padding_start;
            else
                nextstate <= rcv;
            end if;
            
        when zeropad =>
            q.keccakd.en <= '1';
            
            if d.keccakq.ready = '0'
            then
                nextstate <= permute;
            end if;
            
        when permute =>
            q.keccakd.en <= '0';
            shacntd.rst <= '1';
            
            if d.keccakq.ready = '1'
            then
                nextstate <= absorb;
            end if;
            
        when padding_start =>
            q.keccakd.en <= '1';
            shacntd.en <= '1';
            
            nextstate <= padding_finish;
            if shacntq.max = '1'
            then
                nextstate <= zeropad_squeeze;
            end if;
            
        when padding_finish =>
            q.keccakd.en <= '1';
            shacntd.en <= '1';
            
            if shacntq.max = '1'
            then
                nextstate <= zeropad_squeeze;
            end if;
            
        when zeropad_squeeze =>
            q.keccakd.en <= '1';
            
            if d.keccakq.ready = '0'
            then
                nextstate <= permute_squeeze;
            end if;
            
        when permute_squeeze =>
            q.keccakd.en <= '0';
            shacntd.rst <= '1';
            
            if d.keccakq.ready = '1'
            then
                nextstate <= squeeze;
            end if;
            
        when squeeze =>
            q.keccakd.en <= '1';
            shacntd.en <= '1';
            q.muregd.en_rotate <= '1';
            q.muregd.en_write <= '1';
            
            if shacnt = 512/32-1
            then
                nextstate <= reset;
            end if;
        
        when reset =>
            q.keccakd.rst <= '1';
            nextstate <= idle;
            
    end case;

end process;

end Behavioral;
