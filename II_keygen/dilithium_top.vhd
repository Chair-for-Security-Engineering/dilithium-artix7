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
use work.memmap.all;

entity dilithium_top is
    Port (
        clk             : in std_logic;
        op_in           : in std_logic_vector(3 downto 0);
        op_valid_in     : in std_logic;
        ready_out       : out std_logic;
        
        -- IO
        data_in         : in std_logic_vector(31 downto 0);
        ready_rcv_in    : in std_logic;
        valid_in        : in std_logic;
        data_out        : out std_logic_vector(31 downto 0);
        ready_rcv_out   : out std_logic;
        valid_out       : out std_logic
    );
end dilithium_top;

architecture Behavioral of dilithium_top is

    -- opcode register
    signal opreg : std_logic_vector(3 downto 0);
    signal opreg_en : std_logic;
    type state_type is (idle, start, run);
    signal state, nextstate : state_type;

    -- memory
    signal memd, memdreg : memory_in_type := (others => ZEROMEM);
    signal memq, memqreg : memory_out_type;
    
    -- OP interfaces
    signal keygend : keygen_in_type;
    signal keygenq : keygen_out_type;
    signal loadd : load_in_type;
    signal loadq : load_out_type;
    signal stored : store_in_type;
    signal storeq : store_out_type;
    
    -- module interfaces
    signal keccakd : keccak_in_type;
    signal keccakq : keccak_out_type;
    signal nttd : ntt_in_type;
    signal nttq : ntt_out_type;
    signal maccd : macc_poly_in_type;
    signal maccq : macc_poly_out_type;
    signal expandAd, expands1s2d : expand_in_type;
    signal expandAq, expands1s2q : expand_out_type;
    signal expandyd : expand_y_in_type;
    signal expandyq : expand_y_out_type;
    signal matmuld : matmul_in_type;
    signal matmulq : matmul_out_type;
    signal crtd : crh_rho_t1_in_type;
    signal crtq : crh_rho_t1_out_type;
    signal convyzd : convert_yz_in_type;
    signal convyzq : payload_array(0 to 3);
    
    -- global register interfaces
    signal rhoregd, rhoprimeregd, Kregd, trregd, seedregd, chashregd, muregd : reg32_in_type;
    signal rhoregq, rhoprimeregq, Kregq, trregq, seedregq, chashregq, muregq : reg32_out_type;
    signal hregd : hreg_in_type;
    signal hregq : hreg_out_type;

    -- global fifo interfaces
    signal fifoyzd : fifo_in_type;
    signal fifoyzdataq : std_logic_vector((DILITHIUM_loggamma1+1)*4-1 downto 0);
    signal fifoyzq : fifo_out_type;
    signal fifoyzdatad : std_logic_vector(31 downto 0);
    signal fifo160d : fifo_in_type;
    signal fifo160dataq : std_logic_vector(39 downto 0);
    signal fifo160q : fifo_out_type;
    signal fifo160datad : std_logic_vector(31 downto 0);
    signal fifot0d : fifo_in_type;
    signal fifot0dataq : std_logic_vector(51 downto 0);
    signal fifot0q : fifo_out_type;
    signal fifot0datad : std_logic_vector(31 downto 0);
begin

------------------------------------------------------------------------------------
-- op decoding
------------------------------------------------------------------------------------
op_register: process(clk)
begin
    if rising_edge(clk)
    then
        if opreg_en = '1'
        then
            opreg <= op_in;
        end if;
    end if;
end process;

states: process(clk)
begin
    if rising_edge(clk)
    then
        state <= nextstate;
    end if;
end process;

signals: process(state, op_in, opreg, op_valid_in, storeq, loadq, keygenq)
begin
    nextstate <= state;
    
    ready_out <= '0';
    
    opreg_en <= '0';
    stored.en <= '0';
    stored.rst <= '0';
    loadd.en <= '0';
    keygend.en <= '0';
    
    case state is 
        when idle =>
            ready_out <= '1';
            opreg_en <= op_valid_in;
            
            stored.rst <= '1';
    
            if op_valid_in = '1'
            then
                nextstate <= start;
            end if;
        
        when start =>
            if opreg(3) = '1'
            then
                case opreg(3 downto 2) is
                    when OPCODE_STOR(3 downto 2) => 
                        stored.en <= '1';
                        if storeq.ready = '0'
                        then
                            nextstate <= run;
                        end if;
                    when OPCODE_LOAD(3 downto 2) =>
                        loadd.en <= '1';
                        if loadq.ready = '0'
                        then
                            nextstate <= run;
                        end if;
                    when others => nextstate <= idle;
                end case;
            else
                case opreg is
                    when OPCODE_KGEN =>
                        keygend.en <= '1';
                        if keygenq.ready = '0'
                        then
                            nextstate <= run;
                        end if;
                    when others => nextstate <= idle;
                end case;
            end if;
            
        when run =>
            if opreg(3) = '1'
            then
                case opreg(3 downto 2) is
                    when OPCODE_STOR(3 downto 2) => 
                        if storeq.ready = '1'
                        then
                            nextstate <= idle;
                        end if;
                    when OPCODE_LOAD(3 downto 2) =>
                        if loadq.ready = '1'
                        then
                            nextstate <= idle;
                        end if;
                    when others => nextstate <= idle;
                end case;
            else
                case opreg is
                    when OPCODE_KGEN =>
                        if keygenq.ready = '1'
                        then
                            nextstate <= idle;
                        end if;
                    when others => nextstate <= idle;
                end case;
            end if;
    end case;
end process;

------------------------------------------------------------------------------------
-- memory controller
------------------------------------------------------------------------------------
memctrl: entity work.memory
port map (
    clk => clk,
    d => memdreg,
    q => memq
);

memreg: process(clk)
begin
    if rising_edge(clk)
    then
        memdreg <= memd;
        memqreg <= memq;
    end if;
end process;

------------------------------------------------------------------------------------
-- keygen module
------------------------------------------------------------------------------------
keygenctrl: entity work.keygen
port map (
    clk => clk,
    d => keygend,
    q => keygenq
);

keygend.keccakq <= keccakq;
keygend.memq <= memqreg;
keygend.nttq <= nttq;
keygend.maccq <= maccq;
keygend.expandAq <= expandAq;
keygend.expands1s2q <= expands1s2q;
keygend.matmulq <= matmulq;
keygend.crtq <= crtq;
keygend.convyzq <= convyzq;
keygend.rhoregq <= rhoregq;
keygend.Kregq <= Kregq;
keygend.trregq <= trregq;
keygend.seedregq <= seedregq;
keygend.rhoprimeregq <= rhoprimeregq;

------------------------------------------------------------------------------------
-- store module
------------------------------------------------------------------------------------
storectrl: entity work.store
port map (
    clk => clk,
    d => stored,
    q => storeq
);

stored.valid <= valid_in;
stored.payload <= data_in;
stored.payload_type <= opreg(1 downto 0);
stored.convyzq <= convyzq;
stored.zfifoq <= fifoyzq;
stored.zfifodataq <= fifoyzdataq;
stored.fifo160q <= fifo160q;
stored.fifo160dataq <= fifo160dataq;
stored.fifot0q <= fifot0q;
stored.fifot0dataq <= fifot0dataq;

------------------------------------------------------------------------------------
-- load module
------------------------------------------------------------------------------------
loadctrl: entity work.load
port map (
    clk => clk,
    d => loadd,
    q => loadq
);

loadd.valid <= valid_in;
loadd.payload_type <= opreg(1 downto 0);
loadd.ready_rcv <= ready_rcv_in;
loadd.hregq <= hregq;
loadd.memq <= memqreg;
loadd.convyzq <= convyzq;
loadd.rhoregq <= rhoregq;
loadd.Kregq <= Kregq;
loadd.trregq <= trregq;
loadd.chashregq <= chashregq;

------------------------------------------------------------------------------------
-- global modules
------------------------------------------------------------------------------------
matmulctrl: entity work.matmul
port map (
    clk => clk,
    d => matmuld,
    q => matmulq
);

maccctrl: entity work.macc_poly
port map (
    clk => clk,
    d => maccd,
    q => maccq
);

nttctrl: entity work.ntt
port map (
    clk => clk,
    d => nttd,
    q => nttq
);

keccakctrl: entity work.keccak
port map (
    clk => clk,
    d => keccakd,
    q => keccakq
);

expand_A_ctrl: entity work.expandA
port map (
    clk => clk,
    d => expandAd,
    q => expandAq
);

expand_s1s2_ctrl: entity work.expands1s2
port map (
    clk => clk,
    d => expands1s2d,
    q => expands1s2q
);

expand_y_ctrl: entity work.expand_y
port map (
    clk => clk,
    d => expandyd,
    q => expandyq
);

crh_rho_t1_ctrl: entity work.crh_rho_t1
port map (
    clk => clk,
    d => crtd,
    q => crtq
);

convert_yz_ctrl: entity work.convert_yz
port map (
    clk => clk,
    d => convyzd,
    q => convyzq
);
   
------------------------------------------------------------------------------------
-- global registers
------------------------------------------------------------------------------------
rhoreg: entity work.reg32
generic map (width => 256)
port map (
    clk => clk,
    d => rhoregd,
    q => rhoregq
);
rhoprimereg: entity work.reg32
generic map (width => 512)
port map (
    clk => clk,
    d => rhoprimeregd,
    q => rhoprimeregq
);
Kreg: entity work.reg32
generic map (width => 256)
port map (
    clk => clk,
    d => Kregd,
    q => Kregq
);
trreg: entity work.reg32
generic map (width => 256)
port map (
    clk => clk,
    d => trregd,
    q => trregq
);
chashreg: entity work.reg32
generic map (width => 256)
port map (
    clk => clk,
    d => chashregd,
    q => chashregq
);
seedreg: entity work.reg32
generic map (width => 256)
port map (
    clk => clk,
    d => seedregd,
    q => seedregq
);
mureg: entity work.reg32
generic map (width => 512)
port map (
    clk => clk,
    d => muregd,
    q => muregq
);
hregister: entity work.hreg
port map (
    clk => clk,
    d => hregd,
    q => hregq
);

------------------------------------------------------------------------------------
-- global fifos
------------------------------------------------------------------------------------
fifoyz: entity work.fifo
generic map (
    buf_width => 288 - (DILITHIUM_loggamma1/19)*128,
    input_length => 32,
    output_length => (DILITHIUM_loggamma1+1)*4
)
port map (
    clk => clk,
    d => fifoyzd,
    datad => fifoyzdatad,
    q => fifoyzq,
    dataq => fifoyzdataq
);

fifo160: entity work.fifo
generic map (
    buf_width => 160,
    input_length => 32,
    output_length => 40
)
port map (
    clk => clk,
    d => fifo160d,
    datad => fifo160datad,
    q => fifo160q,
    dataq => fifo160dataq
);

fifot0: entity work.fifo
generic map (buf_width => 416, input_length => 32, output_length => 52)
port map (
    clk => clk,
    d => fifot0d,
    datad => fifot0datad,
    q => fifot0q,
    dataq => fifot0dataq
);

------------------------------------------------------------------------------------
-- one big mux
------------------------------------------------------------------------------------
mux: process(opreg, storeq, loadq, keygenq)
begin
    ready_rcv_out <= '0';
    valid_out <= '0';
    data_out <= (others => '0');

    memd <= keygenq.memd;
    
    chashregd <= ZEROREG32;
    rhoregd <= ZEROREG32;
    Kregd <= ZEROREG32;
    trregd <= ZEROREG32;
    seedregd <= ZEROREG32;
    muregd <= ZEROREG32;
    rhoprimeregd <= ZEROREG32;
    hregd <= ZEROHREG;
    fifoyzd <= (others => '0');
    fifoyzdatad <= (others => '0');
    fifo160d <= (others => '0');
    fifo160datad <= (others => '0');
    fifot0d <= (others => '0');
    fifot0datad <= (others => '0');
    
    -- todo zero-init everything
    keccakd <= keygenq.keccakd;
    nttd <= keygenq.nttd;
    maccd <= keygenq.maccd;
    expandAd <= keygenq.expandAd;
    expands1s2d <= keygenq.expands1s2d;
    matmuld <= keygenq.matmuld;
    crtd <= keygenq.crtd;
    convyzd <= keygenq.convyzd;

    if opreg(3) = '1'
    then
        case opreg(3 downto 2) is
            when OPCODE_STOR(3 downto 2) =>
                ready_rcv_out <= storeq.ready_rcv;
                
                memd <= storeq.memd;
                rhoregd <= storeq.rhoregd;
                Kregd <= storeq.Kregd;
                trregd <= storeq.trregd;
                chashregd <= storeq.chashregd;
                hregd <= storeq.hregd;
                seedregd <= storeq.seedregd;
                convyzd <= storeq.convyzd;
                fifoyzd <= storeq.zfifod;
                fifoyzdatad <= storeq.zfifodatad;
                fifo160d <= storeq.fifo160d;
                fifo160datad <= storeq.fifo160datad;
                fifot0d <= storeq.fifot0d;
                fifot0datad <= storeq.fifot0datad;
                
            when OPCODE_LOAD(3 downto 2) =>
                memd <= loadq.memd;
                valid_out <= loadq.valid;
                data_out <= loadq.payload;
                hregd <= loadq.hregd;
                convyzd <= loadq.convyzd;
                rhoregd <= loadq.rhoregd;
                Kregd <= loadq.Kregd;
                trregd <= loadq.trregd;
                chashregd <= loadq.chashregd;
                
                
            when others =>
        end case;
    else
        case opreg is
                
            when OPCODE_KGEN =>
                memd <= keygenq.memd;
                keccakd <= keygenq.keccakd;
                nttd <= keygenq.nttd;
                maccd <= keygenq.maccd;
                expandAd <= keygenq.expandAd;
                expands1s2d <= keygenq.expands1s2d;
                matmuld <= keygenq.matmuld;
                crtd <= keygenq.crtd;
                convyzd <= keygenq.convyzd;
                rhoregd <= keygenq.rhoregd;
                Kregd <= keygenq.Kregd;
                trregd <= keygenq.trregd;
                seedregd <= keygenq.seedregd;
                rhoprimeregd <= keygenq.rhoprimeregd;
                
            when others =>
        end case;
    end if;

end process;

end Behavioral;
