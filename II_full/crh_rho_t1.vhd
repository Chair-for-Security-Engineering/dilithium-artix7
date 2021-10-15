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

entity crh_rho_t1 is
    Port (
        clk : in std_logic;
        d   : in crh_rho_t1_in_type;
        q   : out crh_rho_t1_out_type
    );
end crh_rho_t1;

architecture Behavioral of crh_rho_t1 is

    constant MEMORY_DELAY : natural := GLOBAL_MEMORY_DELAY;

    type state_type is (idle, absorb_rho, read, absorb, zeropad, zeropad_squeeze, permute, permute_squeeze, padding_start, padding_mid, padding_end, squeeze);
    signal state, nextstate : state_type := idle;
    
    signal memcntd, abscntd, absregcntd, rhoabscntd, sqzcntd, readcntd : counter_in_type;
    signal memcntq, abscntq, absregcntq, rhoabscntq, sqzcntq, readcntq : counter_out_type;
    signal memcnt : natural range 0 to DILITHIUM_k*DILITHIUM_N/4-1;
    signal abscnt : natural range 0 to SHAKE256_RATE/32-1;
    signal readcnt : natural range 0 to 160/40+MEMORY_DELAY-1;
    
    type pipeline_array is array(1 to MEMORY_DELAY) of natural range 0 to NUM_MEM_8_POLY-1;
    signal pipeline : pipeline_array;
    signal pipeline_out, pipeline_in : natural range 0 to NUM_MEM_8_POLY-1;
    
    signal addr_rev, addr_bitrev : coef_addr_array(0 to 3);
    
    signal absorbreg : std_logic_vector(159 downto 0);
    signal absorbreg_absorb_en, absorbreg_read_en : std_logic;
    signal absorbreg_read_in, absorbreg_read_in_rev : std_logic_vector(39 downto 0);
    signal absorbreg_absorb_out : std_logic_vector(31 downto 0);
    
    signal keccak_en_override, keccak_en_tmp : std_logic;
    
    -- memory
    signal mem : std_logic;
    
begin

areg: process(clk)
begin
    if rising_edge(clk)
    then
        if absorbreg_read_en = '1'
        then
            absorbreg(159 downto 40) <= absorbreg(119 downto 0);
            absorbreg( 39 downto  0) <= absorbreg_read_in;
        elsif absorbreg_absorb_en = '1'
        then
            absorbreg(159 downto 32) <= absorbreg(127 downto 0);
            absorbreg( 31 downto  0) <= (others => '0');
        end if;
    end if;
end process;
absorbreg_absorb_out <= absorbreg(159 downto 128);



pip: process(clk)
begin
    if rising_edge(clk)
    then
        for i in MEMORY_DELAY downto 2
        loop
            pipeline(i) <= pipeline(i-1);
        end loop;
        pipeline(1) <= pipeline_in;
    end if;
end process;
pipeline_in <= memory_map.t1(memcnt / (DILITHIUM_N/4)).memory_index;
pipeline_out <= pipeline(MEMORY_DELAY);

-- memory ctrl
memmux: process(d.memq, pipeline_out, addr_bitrev, memcnt, mem, memcntd.en)
begin
    for i in 0 to 3
    loop
        absorbreg_read_in_rev(i*10+9 downto i*10) <= (others => '0');
    end loop; 
    
    for m in 0 to NUM_MEM_8_POLY-1
    loop
        -- initialize
        q.memd(m) <= ZEROMEM;
        
        if m = memory_map.t1(memcnt / (DILITHIUM_N/4)).memory_index and mem = '1'
        then
            q.memd(m).rsel <= memory_map.t1(memcnt / (DILITHIUM_N/4)).poly_index;
            q.memd(m).raddr <= addr_bitrev;
            q.memd(m).ren <= (others => memcntd.en);
        end if;
        
        if m = pipeline_out and mem = '1'
        then
            for i in 0 to 3
            loop
                absorbreg_read_in_rev(i*10+9 downto i*10) <= d.memq(m)(i)(22 downto 13);
            end loop; 
        end if;
    end loop;
end process;

mgen: for i in 0 to 3
generate
  addr_rev(i)(7 downto 2) <= std_logic_vector(to_unsigned(memcnt mod (DILITHIUM_N/4), 6));
  addr_rev(i)(1 downto 0) <= std_logic_vector(to_unsigned(i, 2));
  revgen: for j in 0 to 7
  generate
    addr_bitrev(i)(7-j) <= addr_rev(i)(j);
  end generate;
end generate;
-- data in: little endian
dingen: for i in 0 to 4
generate
    absorbreg_read_in(i*8+7 downto i*8) <= absorbreg_read_in_rev(39-i*8 downto 40-i*8-8);
end generate;





-- counter
-- counts total number of coefficients read
-- overflow when all are read
memory_counter: entity work.counter
generic map (max_value => DILITHIUM_k*DILITHIUM_N/4-1)
port map (
    clk => clk,
    d => memcntd,
    q => memcntq,
    value => memcnt
);

-- counts total number of 32-bit words shifted into keccak
-- overflow when current block is absorbed and permutation starts
absorb_counter: entity work.counter 
generic map (max_value => SHAKE256_RATE/32-1)
port map (
    clk => clk,
    d => abscntd,
    q => abscntq,
    value => abscnt
);

absorb_reg_counter: entity work.counter 
generic map (max_value => 160/32-1)
port map (
    clk => clk,
    d => absregcntd,
    q => absregcntq,
    value => open
);

-- counts number of 32-bit words of rho to be absorbed into keccak
absorb_rho_counter: entity work.counter
generic map (max_value => 256/32-1)
port map (
    clk => clk,
    d => rhoabscntd,
    q => rhoabscntq,
    value => open
);

-- read counter
read_counter: entity work.counter
generic map (max_value => 160/40+MEMORY_DELAY-1)
port map (
    clk => clk,
    d => readcntd,
    q => readcntq,
    value => readcnt
);



squeeze_counter: entity work.counter
generic map (max_value => 256/32-1)
port map (
    clk => clk,
    d => sqzcntd,
    q => sqzcntq,
    value => open
);




-- tr register
q.trregd.data <= d.keccakq.data;

-- rho register
q.rhoregd.en_write <= '0'; -- read only
q.rhoregd.data <= (others => '0');

-- keccak
with state
select
    q.keccakd.data <= d.rhoregq.data when absorb_rho,
                      x"1f000000" xor d.keccakq.data when padding_start,
                      x"00000080" xor d.keccakq.data when padding_end,
                      absorbreg_absorb_out xor d.keccakq.data when read | absorb,
                      d.keccakq.data when others;
                    

states: process(clk)
begin
    if rising_edge(clk)
    then
        state <= nextstate;
    end if;
end process;

values: process(state, d, rhoabscntq, abscntq, abscnt, absregcntq, memcntq, sqzcntq, readcntq, readcnt)
begin
    nextstate <= state;
    
    -- standard values
    -- output ctrl
    q.ready <= '0';
    
    -- memory enable
    mem <= '1';
            
    -- rho ctrl
    q.rhoregd.en_rotate <= '0';
    
    -- absorbreg ctrl
    absorbreg_read_en <= '0';
    absorbreg_absorb_en <= '0';
    
    -- counter
    rhoabscntd.rst <= '0';
    rhoabscntd.en <= '0';
    memcntd.rst <= '0';
    memcntd.en <= '0';
    abscntd.rst <= '0';
    abscntd.en <= '0';
    absregcntd.rst <= '0';
    absregcntd.en <= '0';
    sqzcntd.rst <= '0';
    sqzcntd.en <= '0';
    readcntd.rst <= '0';
    readcntd.en <= '0';
    
    -- keccak
    q.keccakd.en <= '0';
    q.keccakd.rst <= '0';
    
    -- tr register
    q.trregd.en_rotate <= '0';
    q.trregd.en_write <= '0';
            
    case state is
        when idle =>
            q.ready <= '1';
            mem <= '0';
            
            rhoabscntd.rst <= '1';
            memcntd.rst <= '1';
            abscntd.rst <= '1';
            absregcntd.rst <= '1';
            sqzcntd.rst <= '1';
            readcntd.rst <= '1';
            
            q.keccakd.rst <= '1';
            
            if d.en = '1' and d.keccakq.ready = '1'
            then
                q.keccakd.rst <= '0';
                q.keccakd.en <= '1';
                nextstate <= absorb_rho;
            end if;
        
        when absorb_rho =>
            -- rho ctrl
            q.rhoregd.en_rotate <= '1';
            
            -- counter
            rhoabscntd.en <= '1';
            abscntd.en <= '1';
            
            -- keccak
            q.keccakd.en <= '1';
            
            if rhoabscntq.max = '1'
            then
                nextstate <= read;
            end if;
        
        when read => 
            -- counter
            readcntd.en <= '1';
            absregcntd.rst <= '1';
            
            if readcnt < 160/40
            then
                memcntd.en <= '1';
            end if;
            if readcnt >= MEMORY_DELAY
            then
                absorbreg_read_en <= '1';
            end if;
            
            if readcntq.max = '1'
            then
                nextstate <= absorb;
            end if;
        
        when absorb =>
            -- counter
            abscntd.en <= '1';
            absregcntd.en <= '1';
            readcntd.rst <= '1';
        
            -- keccak
            q.keccakd.en <= '1';
            
            -- absorb reg ctrl
            absorbreg_absorb_en <= '1';
            
            if abscntq.max = '1' -- this block is complete
            then
                nextstate <= zeropad;
            elsif absregcntq.max = '1' -- read next few coefficients
            then
                if memcntq.ovf = '1'
                then
                    nextstate <= padding_start;
                else
                    nextstate <= read;
                end if;
            end if;
        
        when zeropad =>
            -- keccak
            q.keccakd.en <= '1';
            
            if d.keccakq.ready = '0' -- permutation started
            then
                nextstate <= permute;
            end if;
        
        when zeropad_squeeze =>
            -- keccak
            q.keccakd.en <= '1';
            
            if d.keccakq.ready = '0' -- permutation started
            then
                nextstate <= permute_squeeze;
            end if;
        
        when padding_start =>
            -- counter
            abscntd.en <= '1';
            
            -- keccak
            q.keccakd.en <= '1';
            
            nextstate <= padding_mid;
        
        when padding_mid =>
            -- counter
            abscntd.en <= '1';
            
            -- keccak
            q.keccakd.en <= '1';
            
            if abscnt = SHAKE256_RATE/32-2
            then
                nextstate <= padding_end;
            end if;
        
        when padding_end =>
            -- counter
            abscntd.en <= '1';
            
            -- keccak
            q.keccakd.en <= '1';
            
            nextstate <= zeropad_squeeze;
            
        when permute =>
            -- keccak
            q.keccakd.en <= '0';
            
            -- counter
            abscntd.rst <= '1';
            
            if d.keccakq.ready = '1'
            then
                if absregcntq.ovf = '0' -- still some data left
                then
                    nextstate <= absorb;
                elsif memcntq.ovf = '1' -- no data in absorbreg left and everything is read
                then
                    nextstate <= padding_start;
                else
                    nextstate <= read;
                end if;
            end if;
            
        when permute_squeeze =>
            -- keccak
            q.keccakd.en <= '0';
            
            -- counter
            abscntd.rst <= '1';
            
            if d.keccakq.ready = '1'
            then
                nextstate <= squeeze;
            end if;
            
        when squeeze =>
            -- counter
            sqzcntd.en <= '1';
            
            -- keccak
            q.keccakd.en <= '1';
            
            -- tr register
            q.trregd.en_rotate <= '1';
            q.trregd.en_write <= '1';
            
            if sqzcntq.max = '1'
            then
                nextstate <= idle;
            end if;
            
    end case;
    
end process;

end Behavioral;
