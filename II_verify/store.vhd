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

entity store is
    Port (
        clk : in std_logic;
        d   : in store_in_type;
        q   : out store_out_type
    );
end store;

architecture Behavioral of store is

    type state_type is (idle, s_rho, s_t1,
    s_c, s_z, s_z_finish,
    s_h, s_h_rcv, s_h_poly_rcv, s_h_poly);
    signal state, nextstate : state_type;
    
    signal rcntd : counter_in_type;
    signal rcntq : counter_out_type;
    signal rcnt : natural range 0 to 256/32-1;
    
    signal hbuf : std_logic_vector(31 downto 0);
    signal hbuf_en : std_logic;
    signal omegacntd : counter_in_type;
    signal omegacntq : counter_out_type;
    signal omegacnt : natural range 0 to DILITHIUM_omega+DILITHIUM_k-1;
    
    signal memcntd : counter_in_type;
    signal memcntq : counter_out_type;
    signal memcnt : natural range 0 to DILITHIUM_N*DILITHIUM_k/4-1;
    type memcnt_pipeline_type is array(1 to DELAY_CONV_YZ) of natural range 0 to DILITHIUM_N*DILITHIUM_k/4-1;
    signal memcnt_pipeline : memcnt_pipeline_type;
    signal memcnt_en_pipeline : std_logic_vector(1 to DELAY_CONV_YZ) := (others => '0');
    signal memcnt_pipeline_en : std_logic;

begin
--------------------------------------------------------------------------------------------------
-- memory counter
--------------------------------------------------------------------------------------------------
memory_counter: entity work.counter
generic map (max_value => DILITHIUM_N*DILITHIUM_k/4-1)
port map (
    clk => clk,
    d => memcntd,
    q => memcntq,
    value => memcnt
);

--------------------------------------------------------------------------------------------------
-- storing h: omega counter and h buffer
--------------------------------------------------------------------------------------------------
omega_counter: entity work.counter
generic map (max_value => DILITHIUM_omega+DILITHIUM_k-1)
port map (
    clk => clk,
    d => omegacntd,
    q => omegacntq,
    value => omegacnt
);
h_buffer: process(clk)
begin
    if rising_edge(clk)
    then
        if hbuf_en = '1'
        then
            hbuf <= d.payload;
        end if;
    end if;
end process;

--------------------------------------------------------------------------------------------------
-- register counter
--------------------------------------------------------------------------------------------------
reg_counter: entity work.counter
generic map (max_value => 256/32-1)
port map (
    clk => clk,
    d => rcntd,
    q => rcntq,
    value => rcnt
);

--------------------------------------------------------------------------------------------------
-- reg output
--------------------------------------------------------------------------------------------------
q.rhoregd.data <= d.payload;
q.Kregd.data <= d.payload;
q.trregd.data <= d.payload;
q.chashregd.data <= d.payload;
q.seedregd.data <= d.payload;

--------------------------------------------------------------------------------------------------
-- z buffering
--------------------------------------------------------------------------------------------------
q.fifo160datad <= d.payload;

--------------------------------------------------------------------------------------------------
-- t0 and z buffering and conversion
--------------------------------------------------------------------------------------------------
q.zfifodatad <= d.payload;
q.fifot0datad <= d.payload;
convyzdatagen: process(d)
begin
    for i in 0 to 3
    loop
        q.convyzd.data(i)(22 downto DILITHIUM_loggamma1+1) <= (others => '0');
        q.convyzd.data(i)(DILITHIUM_loggamma1 downto 0) <= d.zfifodataq((i+1)*(DILITHIUM_loggamma1+1)-1 downto i*(DILITHIUM_loggamma1+1));
    end loop;
end process;

--------------------------------------------------------------------------------------------------
-- memcnt pipeline
--------------------------------------------------------------------------------------------------
delay_memcnt: process(clk)
begin
    if rising_edge(clk)
    then
        if memcnt_pipeline_en = '1'
        then
            for i in DELAY_CONV_YZ downto 2
            loop
                memcnt_pipeline(i) <= memcnt_pipeline(i-1);
                memcnt_en_pipeline(i) <= memcnt_en_pipeline(i-1);
            end loop;
            memcnt_pipeline(1) <= memcnt;
            memcnt_en_pipeline(1) <= memcntd.en;
        end if;
    end if;
end process;

--------------------------------------------------------------------------------------------------
-- memory mux
--------------------------------------------------------------------------------------------------
mmux: process(state, d, memcnt, memcnt_pipeline, memcnt_en_pipeline)
variable m, p : natural;
variable memcntaddr : coef_addr_array(0 to 3);
variable memcntaddr_rev : coef_addr_array(0 to 3);
begin

    q.memd <= (others => ZEROMEM);
    
    for i in 0 to 3
    loop
        memcntaddr(i) := std_logic_vector(to_unsigned(memcnt mod (DILITHIUM_N/4), 6)) & std_logic_vector(to_unsigned(i, 2));
        for j in 0 to 7
        loop
            memcntaddr_rev(i)(j) := memcntaddr(i)(7-j);
        end loop;
    end loop;
    
    case state is 
        when s_t1 =>
            m := memory_map.t1(memcnt / (DILITHIUM_N/4)).memory_index;
            p := memory_map.t1(memcnt / (DILITHIUM_N/4)).poly_index;
            q.memd(m).wsel <= p;
            q.memd(m).waddr <= memcntaddr_rev;
            q.memd(m).wen <= (others => d.fifo160q.valid);
            for i in 0 to 3
            loop
                q.memd(m).wdata(i)(22 downto 13) <= d.fifo160dataq(i*10+9 downto i*10);
                q.memd(m).wdata(i)(12 downto 0) <= (others => '0');
            end loop;
            
        when s_z | s_z_finish =>
            m := memory_map.zy(memcnt_pipeline(DELAY_CONV_YZ) / (DILITHIUM_N/4)).memory_index;
            p := memory_map.zy(memcnt_pipeline(DELAY_CONV_YZ) / (DILITHIUM_N/4)).poly_index;
            q.memd(m).wsel <= p;
            for i in 0 to 3
            loop
                memcntaddr(i) := std_logic_vector(to_unsigned(memcnt_pipeline(DELAY_CONV_YZ) mod (DILITHIUM_N/4), 6)) & std_logic_vector(to_unsigned(i, 2));
                for j in 0 to 7
                loop
                    q.memd(m).waddr(i)(j) <= memcntaddr(i)(7-j);
                end loop;
            end loop;
            q.memd(m).wen <= (others => memcnt_en_pipeline(DELAY_CONV_YZ));
            q.memd(m).wdata <= d.convyzq;
            
        when others =>
    end case;
end process;

--------------------------------------------------------------------------------------------------
-- state machine
--------------------------------------------------------------------------------------------------
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

signals: process(state, d, hbuf, rcntq, memcntq, memcnt, omegacntq, omegacnt, memcnt_en_pipeline)
begin
    nextstate <= state;
    
    q.ready <= '0';
    q.ready_rcv <= '0';
    
    rcntd.en <= '0';
    rcntd.rst <= '0';
    
    q.rhoregd.en_rotate <= '0';
    q.rhoregd.en_write <= '0';
    q.Kregd.en_rotate <= '0';
    q.Kregd.en_write <= '0';
    q.trregd.en_rotate <= '0';
    q.trregd.en_write <= '0';
    q.chashregd.en_rotate <= '0';
    q.chashregd.en_write <= '0';
    q.seedregd.en_rotate <= '0';
    q.seedregd.en_write <= '0';
    
    q.hregd <= ZEROHREG;
    
    q.fifot0d.en <= '1'; -- keep fifo in load state...
    q.fifot0d.rst <= '0';
    q.fifot0d.valid <= '0'; -- valid is 0 anyways
    q.fifot0d.ready_rcv <= '0';
    
    omegacntd.en <= '0';
    omegacntd.rst <= '0';
    hbuf_en <= '0';
    
    q.zfifod.en <= '1'; -- keep fifo in load state...
    q.zfifod.rst <= '0';
    q.zfifod.valid <= '0'; -- valid is 0 anyways
    q.zfifod.ready_rcv <= '0';
    q.convyzd.en <= '0';
    q.convyzd.sub <= (others => '0');
    
    q.fifo160d.en <= '1'; -- keep fifo in load state...
    q.fifo160d.rst <= '0';
    q.fifo160d.valid <= '0'; -- valid is 0 anyways
    q.fifo160d.ready_rcv <= '0';
    
    memcntd.en <= '0';
    memcntd.rst <= '0';
    memcnt_pipeline_en <= '0';
    
    case state is
        when idle => 
            q.ready <= '1';
            
            q.zfifod.rst <= '1';
            q.fifo160d.rst <= '1';
            q.fifot0d.rst <= '1';
            
            rcntd.rst <= '1';
            omegacntd.rst <= '1';
            memcntd.rst <= '1';
    
            
            if d.en = '1'
            then
                case d.payload_type is
                    when PAYLOAD_TYPE_PK => nextstate <= s_rho;
                    when PAYLOAD_TYPE_SIG => nextstate <= s_c;
                    when others =>
                end case;
            end if;
        
        -----------------------------------------------------------------------------
        -- store rho
        -----------------------------------------------------------------------------
        when s_rho => 
            q.ready_rcv <= '1';
            
            rcntd.en <= d.valid;
            q.rhoregd.en_rotate <= d.valid;
            q.rhoregd.en_write <= d.valid;
            
            if rcntq.max = '1' and d.valid = '1'
            then
                q.ready_rcv <= '0';
                case d.payload_type is
                    when PAYLOAD_TYPE_PK => nextstate <= s_t1;
                    when others => nextstate <= idle; -- error
                end case;
            end if;
        
        -----------------------------------------------------------------------------
        -- store t1
        -----------------------------------------------------------------------------
        when s_t1 =>
            q.ready_rcv <= d.fifo160q.ready_rcv and not d.fifo160q.toggle;
            q.fifo160d.valid <= d.valid;
            q.fifo160d.ready_rcv <= '1';
            memcntd.en <= d.fifo160q.valid;
            
            if memcntq.max = '1' and d.fifo160q.valid = '1'
            then
                nextstate <= idle; -- loading pk done
            end if;
        
        -----------------------------------------------------------------------------
        -- store c
        -----------------------------------------------------------------------------
        when s_c =>
            q.ready_rcv <= '1';
            rcntd.en <= d.valid;
            q.chashregd.en_rotate <= d.valid;
            q.chashregd.en_write <= d.valid;
            
            memcntd.rst <= '1'; -- prepare for s_z
            
            if rcntq.max = '1'
            then
                q.ready_rcv <= '0';
                nextstate <= s_z;
            end if;
        
        -----------------------------------------------------------------------------
        -- store z
        -----------------------------------------------------------------------------
        when s_z =>
            q.ready_rcv <= d.zfifoq.ready_rcv;
            q.zfifod.valid <= d.valid;
            q.zfifod.ready_rcv <= '1';
            q.convyzd.en <= '1';
            q.convyzd.sub <= std_logic_vector(to_unsigned(2**DILITHIUM_loggamma1, 23));
            memcntd.en <= d.zfifoq.valid;
            memcnt_pipeline_en <= '1';
            
            if memcnt = DILITHIUM_l*DILITHIUM_N/4-1
            then
                nextstate <= s_z_finish;
            end if;
            
        when s_z_finish =>
            q.convyzd.en <= '1';
            q.convyzd.sub <= std_logic_vector(to_unsigned(2**DILITHIUM_loggamma1, 23));
            memcnt_pipeline_en <= '1';
            if memcnt_pipeline(DELAY_CONV_YZ) = DILITHIUM_l*DILITHIUM_N/4-1
            then
                nextstate <= s_h_rcv;
            end if;
        
        -----------------------------------------------------------------------------
        -- unpack h
        -----------------------------------------------------------------------------
        when s_h_rcv => 
            q.ready_rcv <= '1';
            hbuf_en <= d.valid;
            
            if d.valid = '1'
            then
                q.ready_rcv <= '0';
                nextstate <= s_h;
            end if;
        
        when s_h => 
            omegacntd.en <= '1';
            q.hregd.en_rotate_data <= '1';
            q.hregd.en_write_data <= '1';
            q.hregd.data_offset <= hbuf(31-(omegacnt mod 4)*8 downto 32-(omegacnt mod 4)*8-8);
            
            if (omegacnt mod 4) = 3
            then
                if omegacnt = DILITHIUM_omega-1
                then
                    nextstate <= s_h_poly_rcv;
                else
                    nextstate <= s_h_rcv;
                end if;
            elsif omegacnt = DILITHIUM_omega-1
            then
                nextstate <= s_h_poly;
            end if; 
        
        when s_h_poly_rcv =>
            q.ready_rcv <= '1';
            hbuf_en <= d.valid;
            
            if d.valid = '1'
            then
                q.ready_rcv <= '0';
                nextstate <= s_h_poly;
            end if;
        
        when s_h_poly =>
            omegacntd.en <= '1';
            q.hregd.en_rotate_poly <= '1';
            q.hregd.en_write_poly <= '1';
            q.hregd.poly_offset <= hbuf(31-(omegacnt mod 4)*8 downto 32-(omegacnt mod 4)*8-8);
            
            if omegacntq.max = '1'
            then
                nextstate <= idle; -- DONE loading signature
            elsif (omegacnt mod 4) = 3
            then
                nextstate <= s_h_poly_rcv;
            end if;
        
        
        
        
        when others => nextstate <= idle;
    end case;

end process;

end Behavioral;
