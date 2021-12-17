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
    signal vrfyd : verify_in_type;
    signal vrfyq : verify_out_type;
    signal prevrfyd : verify_precomp_in_type;
    signal prevrfyq : verify_precomp_out_type;
    signal dmsgd : digest_msg_in_type;
    signal dmsgq : digest_msg_out_type;
    signal stored : store_in_type;
    signal storeq : store_out_type;
    
    -- module interfaces
    signal keccakd : keccak_in_type;
    signal keccakq : keccak_out_type;
    signal nttd : ntt_in_type;
    signal nttq : ntt_out_type;
    signal maccd : macc_poly_in_type;
    signal maccq : macc_poly_out_type;
    signal expandAd : expand_in_type;
    signal expandAq : expand_out_type;
    signal matmuld : matmul_in_type;
    signal matmulq : matmul_out_type;
    signal crtd : crh_rho_t1_in_type;
    signal crtq : crh_rho_t1_out_type;
    signal ballsampled : ballsample_in_type;
    signal ballsampleq : ballsample_out_type;
    signal usehintd : use_hint_in_type;
    signal usehintq : use_hint_out_type;
    signal chkzd : check_z_in_type;
    signal chkzq : check_z_out_type;
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

signals: process(state, op_in, opreg, op_valid_in, storeq, vrfyq, prevrfyq, dmsgq)
begin
    nextstate <= state;
    
    ready_out <= '0';
    
    opreg_en <= '0';
    dmsgd.en <= '0';
    stored.en <= '0';
    stored.rst <= '0';
    vrfyd.en <= '0';
    vrfyd.rst <= '0';
    prevrfyd.en <= '0';
    
    case state is 
        when idle =>
            ready_out <= '1';
            opreg_en <= op_valid_in;
            
            stored.rst <= '1';
            vrfyd.rst <= '1';
    
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
                    when others => nextstate <= idle;
                end case;
            else
                case opreg is
                    when OPCODE_DIGEST_MSG =>
                        dmsgd.en <= '1';
                        if dmsgq.ready = '0'
                        then
                            nextstate <= run;
                        end if;
                    when OPCODE_VRFY =>
                        vrfyd.en <= '1';
                        if vrfyq.ready = '0'
                        then
                            nextstate <= run;
                        end if;
                    when OPCODE_VRFY_PRECOMP =>
                        prevrfyd.en <= '1';
                        if prevrfyq.ready = '0'
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
                    when others => nextstate <= idle;
                end case;
            else
                case opreg is
                    when OPCODE_DIGEST_MSG =>
                        if dmsgq.ready = '1'
                        then
                            nextstate <= idle;
                        end if;
                    when OPCODE_VRFY =>
                        if vrfyq.ready = '1'
                        then
                            nextstate <= idle;
                        end if;
                    when OPCODE_VRFY_PRECOMP =>
                        if prevrfyq.ready = '1'
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
-- verify module
------------------------------------------------------------------------------------
vrfyctrl: entity work.verify
port map (
    clk => clk,
    d => vrfyd,
    q => vrfyq
);

vrfyd.ballsampleq <= ballsampleq;
vrfyd.matmulq <= matmulq;
vrfyd.nttq <= nttq;
vrfyd.maccq <= maccq;
vrfyd.keccakq <= keccakq;
vrfyd.memq <= memqreg;
vrfyd.hregq <= hregq;
vrfyd.muregq <= muregq;
vrfyd.chashregq <= chashregq;
vrfyd.usehintq <= usehintq;
vrfyd.chkzq <= chkzq;

------------------------------------------------------------------------------------
-- verify_precomp module
------------------------------------------------------------------------------------
prevrfyctrl: entity work.verify_precomp
port map (
    clk => clk,
    d => prevrfyd,
    q => prevrfyq
);

prevrfyd.keccakq <= keccakq;
prevrfyd.nttq <= nttq;
prevrfyd.memq <= memqreg;
prevrfyd.expandAq <= expandAq;
prevrfyd.crt1q <= crtq;
prevrfyd.rhoregq <= rhoregq;

------------------------------------------------------------------------------------
-- digest message module
------------------------------------------------------------------------------------
dmsgctrl: entity work.digest_msg
port map (
    clk => clk,
    d => dmsgd,
    q => dmsgq
);

dmsgd.payload <= data_in;
dmsgd.valid <= valid_in;
dmsgd.done <= ready_rcv_in; -- a bit messy... :/
dmsgd.keccakq <= keccakq;
dmsgd.trregq <= trregq;

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

hint_ctrl: entity work.use_hint
port map (
    clk => clk,
    d => usehintd,
    q => usehintq
);
    
ballsamplectrl: entity work.ballsample
port map (
    clk => clk,
    d => ballsampled,
    q => ballsampleq
);

zcheckctrl: entity work.check_z
port map (
    clk => clk,
    d => chkzd,
    q => chkzq
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
mux: process(opreg, storeq, vrfyq, prevrfyq, dmsgq)
begin
    ready_rcv_out <= '0';
    valid_out <= '0';
    data_out <= (others => '0');

    memd <= (others => ZEROMEM);
    
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
    keccakd <= vrfyq.keccakd;
    nttd <= vrfyq.nttd;
    maccd <= vrfyq.maccd;
    expandAd <= prevrfyq.expandAd;
    matmuld <= vrfyq.matmuld;
    crtd <= prevrfyq.crt1d;
    convyzd <= storeq.convyzd;
    ballsampled <= vrfyq.ballsampled;
    usehintd <= vrfyq.usehintd;
    chkzd <= vrfyq.chkzd;

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
                
                
            when others =>
        end case;
    else
        case opreg is
            when OPCODE_DIGEST_MSG =>
                ready_rcv_out <= dmsgq.ready_rcv;
                keccakd <= dmsgq.keccakd;
                trregd <= dmsgq.trregd;
                muregd <= dmsgq.muregd;
            
            when OPCODE_VRFY =>
                memd <= vrfyq.memd;
                hregd <= vrfyq.hregd;
                ballsampled <= vrfyq.ballsampled;
                usehintd <= vrfyq.usehintd;
                chkzd <= vrfyq.chkzd;
                matmuld <= vrfyq.matmuld;
                nttd <= vrfyq.nttd;
                maccd <= vrfyq.maccd;
                keccakd <= vrfyq.keccakd;
                muregd <= vrfyq.muregd;
                chashregd <= vrfyq.chashregd;
                ready_rcv_out <= vrfyq.result; -- messy :/
                valid_out <= vrfyq.valid;
                
            when OPCODE_VRFY_PRECOMP =>
                keccakd <= prevrfyq.keccakd;
                memd <= prevrfyq.memd;
                nttd <= prevrfyq.nttd;
                expandAd <= prevrfyq.expandAd;
                crtd <= prevrfyq.crt1d;
                rhoregd <= prevrfyq.rhoregd;
                trregd <= prevrfyq.trregd;
                
            when others =>
        end case;
    end if;

end process;

end Behavioral;
