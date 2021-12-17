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

entity sign is
    Port (
        clk : in std_logic;
        d : in sign_in_type;
        q : out sign_out_type
    );
end sign;

architecture Behavioral of sign is

type state_type is (idle,
absorbK, absorbmu, padding, zeropad, permute, squeeze,
expand_y, expand_y_finish, 
matmul, matmul_finish, 
inttw, inttw_finish, inttw_shortcut,
highbits, highbits_finish, 
ballsample, ballsample_finish,
nttc, nttc_finish,
y_cs1, y_cs1_finish, y_cs1_shortcut,
inttz, inttz_finish, inttz_shortcut,
check_z, check_z_finish,
cs2, cs2_finish, cs2_shortcut,
inttcs2, inttcs2_finish, inttcs2_shortcut,
sub_from_w, sub_from_w_finish, sub_from_w_shortcut,
lowbits, lowbits_finish, lowbits_shortcut,
ct0, ct0_finish, ct0_shortcut,
inttct0, inttct0_finish, inttct0_shortcut,
check_ct0, check_ct0_finish,
add_wcs2, add_wcs2_finish, add_wcs2_shortcut,
makehint, makehint_flush,
restart
);
signal state, nextstate : state_type;

signal shacntd, kcntd, lcntd, memcntd, omegacntd : counter_in_type;
signal shacntq, kcntq, lcntq, memcntq, omegacntq : counter_out_type;
signal shacnt : natural range 0 to SHAKE256_RATE/32-1;
signal kcnt : natural range 0 to DILITHIUM_k-1;
signal lcnt : natural range 0 to DILITHIUM_l-1;
signal memcnt : natural range 0 to DILITHIUM_k*DILITHIUM_N+GLOBAL_MEMORY_DELAY+1-1;
signal memcnt_delayed : natural range 0 to DILITHIUM_k*DILITHIUM_N-1;
signal omegacnt : natural range 0 to DILITHIUM_omega-1;

signal kappa_inc, kappa_rst : std_logic;
signal kappa : natural range 0 to 2**16-1;

signal keccakd_sign : keccak_in_type;
signal nttd_sign : ntt_in_type;
signal maccd_sign : macc_poly_in_type;
signal memd_sign : memory_in_type;

signal hb2glutd, hb2glutq : payload_array(0 to 3);
signal lowbits_valid : std_logic := '1';

signal ct0_valid : std_logic := '1';

-- make hint
signal highwcs2d, highct0d, highwcs2q, highct0q : std_logic_vector(22 downto 0);
signal hint : std_logic;
    
type memi_type is array(0 to 5) of natural range 0 to NUM_MEM_8_POLY;
type pipeline_type is record
    en : std_logic_vector(0 to 5);
    memi : memi_type;
end record pipeline_type;
type pipeline_array is array(1 to GLOBAL_MEMORY_DELAY) of pipeline_type;
signal pip_in, pip_out : pipeline_type := (en => "000000", memi => (others => 0));
signal pipeline : pipeline_array := (others => (en => "000000", memi => (others => 0)));

signal muregd_sign, rhoprimeregd_sign, chashregd_sign : reg32_in_type := ZEROREG32;

begin
----------------------------------------------------------------------------------------------------------
-- make hint stuff
----------------------------------------------------------------------------------------------------------
highbitslut: entity work.highbits
port map (
    clk => clk,
    d => highwcs2d,
    q => highwcs2q
);
highbitslut2: entity work.highbits
port map (
    clk => clk,
    d => highct0d,
    q => highct0q
);
hint <= '1' when highwcs2q /= highct0q or state = makehint_flush else '0';
omegacntd.en <= hint;
q.hregd.en_rotate_data <= hint;
q.hregd.en_write_data  <= hint;
memcnt_delayed <= memcnt - GLOBAL_MEMORY_DELAY - 1 when memcnt >= GLOBAL_MEMORY_DELAY + 1 else 0;
q.hregd.data_offset <= x"00" when state = makehint_flush else std_logic_vector(to_unsigned(memcnt_delayed mod DILITHIUM_N, 8));
q.hregd.en_rotate_poly <= '1' when ((memcnt_delayed/DILITHIUM_N) /= 0 and (memcnt_delayed mod DILITHIUM_N) = 0) or memcntq.max = '1' else '0';
q.hregd.en_write_poly  <= '1' when ((memcnt_delayed/DILITHIUM_N) /= 0 and (memcnt_delayed mod DILITHIUM_N) = 0) or memcntq.max = '1' else '0';
q.hregd.poly_offset <= std_logic_vector(to_unsigned(omegacnt, 8));-- when memcntq.max = '0' else std_logic_vector(to_unsigned(omegacnt+1, 8));

----------------------------------------------------------------------------------------------------------
-- highbits * 2*gamma2 LUT module
----------------------------------------------------------------------------------------------------------
high2gammalut: entity work.highbits2gamma
port map (
    d => hb2glutd,
    q => hb2glutq
);
lbvalid: process(clk)
begin
    if rising_edge(clk)
    then
        lowbits_valid <= '1';
        case state is
            when lowbits | lowbits_shortcut | lowbits_finish =>
                for i in 0 to 3
                loop
                    if d.maccq.wen = '1' 
                    and unsigned(d.maccq.wdata(i)) >= DILITHIUM_gamma2-DILITHIUM_beta 
                    and unsigned(d.maccq.wdata(i)) <= DILITHIUM_Q - DILITHIUM_gamma2 + DILITHIUM_beta
                    then
                        lowbits_valid <= '0';
                    end if;
                end loop;
            when others =>
        end case;
    end if;
end process;

----------------------------------------------------------------------------------------------------------
-- module io
----------------------------------------------------------------------------------------------------------
q.expandyd.keccakq <= d.keccakq;
q.expandyd.convertyzq <= d.convyzq;
q.expandyd.fifoyzq <= d.fifoyzq;
q.expandyd.fifoyzdataq <= d.fifoyzdataq;
q.convyzd <= d.expandyq.convertyzd;
q.fifoyzd <= d.expandyq.fifoyzd;
q.fifoyzdatad <= d.expandyq.fifoyzdatad;
q.ballsampled.keccakq <= d.keccakq;
q.ballsampled.seedregq <= d.chashregq;
q.matmuld.nttq <= d.nttq;
q.matmuld.maccq <= d.maccq;
q.usehintd.muregq <= d.muregq;
q.usehintd.keccakq <= d.keccakq;
q.expandyd.seedregq <= d.rhoprimeregq;
rhoprimeregd_sign.data <= d.keccakq.data;

----------------------------------------------------------------------------------------------------------
-- keccak, ntt, macc, register muxes
----------------------------------------------------------------------------------------------------------
with state
select
    q.keccakd <=    d.expandyq.keccakd when expand_y | expand_y_finish,
                    d.ballsampleq.keccakd when ballsample | ballsample_finish,
                    d.usehintq.keccakd when highbits | highbits_finish,
                    keccakd_sign when others;
with state
select
    q.nttd <=   d.matmulq.nttd when matmul | matmul_finish,
                nttd_sign when others;
with state
select
    q.maccd <=  d.matmulq.maccd when matmul | matmul_finish,
                maccd_sign when others;
with state
select
    q.muregd <= d.usehintq.muregd when highbits | highbits_finish,
                muregd_sign when others;
with state
select
    q.rhoprimeregd <= d.expandyq.seedregd when expand_y | expand_y_finish,
                      rhoprimeregd_sign when others;
with state
select
    q.chashregd <= d.usehintq.chashregd when highbits | highbits_finish,
                   d.ballsampleq.seedregd when ballsample | ballsample_finish,
                   chashregd_sign when others;

----------------------------------------------------------------------------------------------------------
-- memory mux
----------------------------------------------------------------------------------------------------------
with state
select
    q.memd <=   d.expandyq.memd when expand_y | expand_y_finish,
                d.matmulq.memd when matmul | matmul_finish,
                d.usehintq.memd when highbits | highbits_finish,
                d.ballsampleq.memd when ballsample | ballsample_finish,
                d.chkzq.memd when check_z | check_z_finish | check_ct0 | check_ct0_finish,
                memd_sign when others;

q.matmuld.memq <= d.memq;
q.usehintd.memq <= d.memq;
q.chkzd.memq <= d.memq; 
                
midelay: process(clk)
begin
    if rising_edge(clk)
    then
        for i in GLOBAL_MEMORY_DELAY downto 2
        loop
            pipeline(i) <= pipeline(i-1);
        end loop;
        pipeline(1) <= pip_in;
    end if;
end process;
pip_out <= pipeline(GLOBAL_MEMORY_DELAY);

mux: process(state, d, kcntq, kcnt, lcntq, lcnt, pip_out, hb2glutq, memcnt, memcntd, memcntq)
variable minttr,minttw,mimca,mimcb,mimcc,pinttr,pinttw,pimca,pimcb,pimcc,mimcr,pimcr : natural;
variable ntten, maccen, maccen_c, maccen_r : boolean;
variable tmpaddr : std_logic_vector(7 downto 0);
begin
    
    minttr := 0;
    minttw := 0;
    mimca := 0;
    mimcb := 0;
    mimcc := 0;
    pinttr := 0;
    pinttw := 0;
    pimca := 0;
    pimcb := 0;
    pimcc := 0;
    mimcr := 0;
    pimcr := 0;
    tmpaddr := "00000000";
    
    memd_sign <= (others => ZEROMEM);
    if pip_out.en(0) = '1'
    then
        nttd_sign.data <= d.memq(pip_out.memi(0));
    else
        nttd_sign.data <= (others => (others => '0'));
    end if;
    if pip_out.en(1) = '1'
    then
        maccd_sign.memq_a <= d.memq(pip_out.memi(1));
        hb2glutd <= d.memq(pip_out.memi(1));
    else
        maccd_sign.memq_a <= (others => (others => '0'));
        hb2glutd <= (others => (others => '0'));
    end if;
    if pip_out.en(2) = '1'
    then
        maccd_sign.memq_b <= d.memq(pip_out.memi(2));
    else
        maccd_sign.memq_b <= (others => (others => '0'));
    end if;
    if pip_out.en(3) = '1'
    then
        maccd_sign.memq_c <= d.memq(pip_out.memi(3));
    else
        maccd_sign.memq_c <= (others => (others => '0'));
    end if;
    if pip_out.en(4) = '1'
    then
        highwcs2d <= d.memq(pip_out.memi(4))(0);
    else
        highwcs2d <= (others => '0');
    end if;
    if pip_out.en(5) = '1'
    then
        highct0d <= d.memq(pip_out.memi(5))(0);
    else
        highct0d <= (others => '0');
    end if;
    
    ntten := false;
    maccen := false;
    maccen_c := false;
    maccen_r := false;
    
    pip_in.en <= (others => '0');
    pip_in.memi <= (others => 0);
    
    case state is
        when nttc | nttc_finish =>
            memd_sign(memory_map.c.memory_index).rsel <= memory_map.c.poly_index; 
            memd_sign(memory_map.c.memory_index).wsel <= memory_map.c.poly_index;
            memd_sign(memory_map.c.memory_index).ren <= (others => d.nttq.ren);
            memd_sign(memory_map.c.memory_index).wen <= (others => d.nttq.wen);
            memd_sign(memory_map.c.memory_index).raddr <= d.nttq.raddr;
            memd_sign(memory_map.c.memory_index).waddr <= d.nttq.waddr;
            memd_sign(memory_map.c.memory_index).wdata <= d.nttq.wdata;
            nttd_sign.data <= d.memq(memory_map.c.memory_index);
            
        when inttw | inttw_finish | inttw_shortcut =>
            ntten := true;
            minttr := memory_map.w(kcnt).memory_index;
            pinttr := memory_map.w(kcnt).poly_index;
            minttw := minttr;
            pinttw := pinttr;
            if state = inttw_shortcut and kcnt > 0 and kcntq.ovf = '0'
            then
                minttw := memory_map.w(kcnt-1).memory_index;
                pinttw := memory_map.w(kcnt-1).poly_index;
            end if;
            
        when inttz | inttz_finish | inttz_shortcut =>
            ntten := true;
            minttr := memory_map.zy(lcnt).memory_index;
            pinttr := memory_map.zy(lcnt).poly_index;
            minttw := minttr;
            pinttw := pinttr;
            if state = inttz_shortcut and lcnt > 0 and lcntq.ovf = '0'
            then
                minttw := memory_map.zy(lcnt-1).memory_index;
                pinttw := memory_map.zy(lcnt-1).poly_index;
            end if;
            
        when inttcs2 | inttcs2_finish | inttcs2_shortcut =>
            ntten := true;
            minttr := memory_map.tmp0(kcnt).memory_index;
            pinttr := memory_map.tmp0(kcnt).poly_index;
            minttw := minttr;
            pinttw := pinttr;
            if state = inttcs2_shortcut and kcnt > 0 and kcntq.ovf = '0'
            then
                minttw := memory_map.tmp0(kcnt-1).memory_index;
                pinttw := memory_map.tmp0(kcnt-1).poly_index;
            end if;
            
        when inttct0 | inttct0_finish | inttct0_shortcut =>
            ntten := true;
            minttr := memory_map.tmp1(kcnt).memory_index;
            pinttr := memory_map.tmp1(kcnt).poly_index;
            minttw := minttr;
            pinttw := pinttr;
            if state = inttct0_shortcut and kcnt > 0 and kcntq.ovf = '0'
            then
                minttw := memory_map.tmp1(kcnt-1).memory_index;
                pinttw := memory_map.tmp1(kcnt-1).poly_index;
            end if;
        
        when y_cs1 | y_cs1_finish | y_cs1_shortcut =>
            maccen := true;
            maccen_c := true;
            maccen_r := true;
            mimca := memory_map.c.memory_index;
            pimca := memory_map.c.poly_index;
            mimcb := memory_map.s1(lcnt).memory_index;
            pimcb := memory_map.s1(lcnt).poly_index;
            mimcc := memory_map.zy(lcnt).memory_index;
            pimcc := memory_map.zy(lcnt).poly_index;
            mimcr := memory_map.zy(lcnt).memory_index;
            pimcr := memory_map.zy(lcnt).poly_index;
            if state = y_cs1_shortcut and lcnt > 0 and lcntq.ovf = '0'
            then
                mimcr := memory_map.zy(lcnt-1).memory_index;
                pimcr := memory_map.zy(lcnt-1).poly_index;
            end if;
        
        when cs2 | cs2_finish | cs2_shortcut =>
            maccen := true;
            maccen_c := false;
            maccen_r := true;
            mimca := memory_map.c.memory_index;
            pimca := memory_map.c.poly_index;
            mimcb := memory_map.s2(kcnt).memory_index;
            pimcb := memory_map.s2(kcnt).poly_index;
            mimcr := memory_map.tmp0(kcnt).memory_index;
            pimcr := memory_map.tmp0(kcnt).poly_index;
            if state = cs2_shortcut and kcnt > 0 and kcntq.ovf = '0'
            then
                mimcr := memory_map.tmp0(kcnt-1).memory_index;
                pimcr := memory_map.tmp0(kcnt-1).poly_index;
            end if;
        
        when sub_from_w | sub_from_w_finish | sub_from_w_shortcut =>
            maccen := true;
            maccen_c := false;
            maccen_r := true;
            mimca := memory_map.tmp0(kcnt).memory_index;
            pimca := memory_map.tmp0(kcnt).poly_index;
            mimcb := memory_map.w(kcnt).memory_index;
            pimcb := memory_map.w(kcnt).poly_index;
            mimcr := memory_map.tmp0(kcnt).memory_index;
            pimcr := memory_map.tmp0(kcnt).poly_index;
            if state = sub_from_w_shortcut and kcnt > 0 and kcntq.ovf = '0'
            then
                mimcr := memory_map.tmp0(kcnt-1).memory_index;
                pimcr := memory_map.tmp0(kcnt-1).poly_index;
            end if;
        
        when ct0 | ct0_finish | ct0_shortcut =>
            maccen := true;
            maccen_c := false;
            maccen_r := true;
            mimca := memory_map.c.memory_index;
            pimca := memory_map.c.poly_index;
            mimcb := memory_map.t0(kcnt).memory_index;
            pimcb := memory_map.t0(kcnt).poly_index;
            mimcr := memory_map.tmp1(kcnt).memory_index;
            pimcr := memory_map.tmp1(kcnt).poly_index;
            if state = ct0_shortcut and kcnt > 0 and kcntq.ovf = '0'
            then
                mimcr := memory_map.tmp1(kcnt-1).memory_index;
                pimcr := memory_map.tmp1(kcnt-1).poly_index;
            end if;
        
        when lowbits | lowbits_finish | lowbits_shortcut =>
            maccen := true;
            maccen_c := false;
            maccen_r := false;
            mimca := memory_map.tmp0(kcnt).memory_index;
            pimca := memory_map.tmp0(kcnt).poly_index;
            mimcb := memory_map.tmp0(kcnt).memory_index;
            pimcb := memory_map.tmp0(kcnt).poly_index;
            if pip_out.en(1) = '1'
            then
                maccd_sign.memq_a <= hb2glutq;
            else
                maccd_sign.memq_a <= (others => (others => '0'));
            end if;
        
        when add_wcs2 | add_wcs2_finish | add_wcs2_shortcut =>
            maccen := true;
            maccen_c := false;
            maccen_r := true;
            mimca := memory_map.tmp1(kcnt).memory_index;
            pimca := memory_map.tmp1(kcnt).poly_index;
            mimcb := memory_map.tmp0(kcnt).memory_index;
            pimcb := memory_map.tmp0(kcnt).poly_index;
            mimcr := memory_map.tmp1(kcnt).memory_index;
            pimcr := memory_map.tmp1(kcnt).poly_index;
            if state = add_wcs2_shortcut and kcnt > 0 and kcntq.ovf = '0'
            then
                mimcr := memory_map.tmp1(kcnt-1).memory_index;
                pimcr := memory_map.tmp1(kcnt-1).poly_index;
            end if;
        
        when makehint =>
            if memcnt < DILITHIUM_k*DILITHIUM_N
            then
                memd_sign(memory_map.tmp0(memcnt/DILITHIUM_N).memory_index).rsel <= memory_map.tmp0(memcnt/DILITHIUM_N).poly_index;
                memd_sign(memory_map.tmp1(memcnt/DILITHIUM_N).memory_index).rsel <= memory_map.tmp1(memcnt/DILITHIUM_N).poly_index;
                memd_sign(memory_map.tmp0(memcnt/DILITHIUM_N).memory_index).ren <= "0001";
                memd_sign(memory_map.tmp1(memcnt/DILITHIUM_N).memory_index).ren <= "0001";
                pip_in.en(4 to 5) <= "11";
                tmpaddr := std_logic_vector(to_unsigned(memcnt mod DILITHIUM_N, 8));
                for i in 0 to 3
                loop
                    for j in 0 to 7
                    loop
                        memd_sign(memory_map.tmp0(memcnt/DILITHIUM_N).memory_index).raddr(i)(j) <= tmpaddr(7-j);
                        memd_sign(memory_map.tmp1(memcnt/DILITHIUM_N).memory_index).raddr(i)(j) <= tmpaddr(7-j);
                    end loop;
                end loop;
                pip_in.memi(4) <= memory_map.tmp0(memcnt/DILITHIUM_N).memory_index;
                pip_in.memi(5) <= memory_map.tmp1(memcnt/DILITHIUM_N).memory_index;
            end if;
        
        
        when others =>
    end case;
    
    if ntten = true
    then
        memd_sign(minttw).wsel <= pinttw;
        memd_sign(minttw).waddr <= d.nttq.waddr;
        memd_sign(minttw).wdata <= d.nttq.wdata;
        memd_sign(minttw).wen <= (others => d.nttq.wen);
        memd_sign(minttr).rsel <= pinttr;
        memd_sign(minttr).raddr <= d.nttq.raddr;
        memd_sign(minttr).ren <= (others => d.nttq.ren);
        pip_in.memi(0) <= minttr;
        pip_in.en(0) <= '1';
    end if;
    
    if maccen = true
    then
        memd_sign(mimca).rsel <= pimca;
        memd_sign(mimca).raddr <= d.maccq.raddr;
        memd_sign(mimca).ren <= (others => d.maccq.ren);
        memd_sign(mimcb).rsel <= pimcb;
        memd_sign(mimcb).raddr <= d.maccq.raddr;
        memd_sign(mimcb).ren <= (others => d.maccq.ren);
        if maccen_r = true
        then
            memd_sign(mimcr).wsel <= pimcr;
            memd_sign(mimcr).waddr <= d.maccq.waddr;
            memd_sign(mimcr).wen <= (others => d.maccq.wen);
            memd_sign(mimcr).wdata <= d.maccq.wdata;
        end if;
        pip_in.memi(1) <= mimca;
        pip_in.memi(2) <= mimcb;
        pip_in.en(1 to 2) <= "11";
    end if;
    if maccen_c = true
    then
        memd_sign(mimcc).rsel <= pimcc;
        memd_sign(mimcc).raddr <= d.maccq.raddr;
        memd_sign(mimcc).ren <= (others => d.maccq.ren);
        pip_in.memi(3) <= mimcc;
        pip_in.en(3) <= '1';
    end if;
end process;

----------------------------------------------------------------------------------------------------------
-- counter
----------------------------------------------------------------------------------------------------------
kappa_counter: process(clk)
begin
    if rising_edge(clk)
    then
        if kappa_rst = '1'
        then
            kappa <= 0;
        elsif kappa_inc = '1'
        then
            kappa <= kappa + DILITHIUM_l;
        end if;
    end if;
end process;
q.expandyd.kappa <= kappa;

sha_counter: entity work.counter
generic map (max_value => SHAKE256_RATE/32-1)
port map (
    clk => clk,
    d => shacntd,
    q => shacntq,
    value => shacnt
);

k_counter: entity work.counter
generic map (max_value => DILITHIUM_k-1)
port map (
    clk => clk,
    d => kcntd,
    q => kcntq,
    value => kcnt
);

l_counter: entity work.counter
generic map (max_value => DILITHIUM_l-1)
port map (
    clk => clk,
    d => lcntd,
    q => lcntq,
    value => lcnt
);

memory_counter: entity work.counter
generic map (max_value => DILITHIUM_k*DILITHIUM_N+GLOBAL_MEMORY_DELAY-1)
port map (
    clk => clk,
    d => memcntd,
    q => memcntq,
    value => memcnt
);

omega_counter: entity work.counter
generic map (max_value => DILITHIUM_omega-1)
port map (
    clk => clk,
    d => omegacntd,
    q => omegacntq,
    value => omegacnt
);

----------------------------------------------------------------------------------------------------------
-- keccak input
----------------------------------------------------------------------------------------------------------
kecin: process(state, d, shacnt)
begin
    
    keccakd_sign.data <= x"00000000";
    
    case state is
        when absorbK => keccakd_sign.data <= d.Kregq.data;
        when absorbmu => keccakd_sign.data <= d.muregq.data;
        when padding =>
            case shacnt is
                when (256 + 512)/32 => keccakd_sign.data <= x"1f000000";
                when SHAKE256_RATE/32-1 => keccakd_sign.data <= x"00000080";
                when others =>
            end case;
        when others =>
    end case;
    
end process;

----------------------------------------------------------------------------------------------------------
-- state machine
----------------------------------------------------------------------------------------------------------
states: process(clk)
begin
    if rising_edge(clk)
    then
        state <= nextstate;
    end if;
end process;

signals: process(state, d, 
shacntq, shacnt, kcntq, kcnt, lcntq, lcnt, memcntq, memcnt, lowbits_valid, omegacntq, pip_out, omegacntq)
begin
    nextstate <= state;
    
    q.ready <= '0';
    
    q.expandyd.en <= '0';
    
    shacntd.en <= '0';
    shacntd.rst <= '0';
    kcntd.en <= '0';
    kcntd.rst <= '0';
    lcntd.en <= '0';
    lcntd.rst <= '0';
    memcntd.en <= '0';
    memcntd.rst <= '0';
    omegacntd.rst <= '0';
    
    keccakd_sign.en <= '0';
    keccakd_sign.rst <= '0';
    
    q.Kregd.en_rotate <= '0';
    q.Kregd.en_write <= '0';
    muregd_sign.en_rotate <= '0';
    muregd_sign.en_write <= '0';
    rhoprimeregd_sign.en_rotate <= '0';
    rhoprimeregd_sign.en_write <= '0';
    
    q.matmuld.en <= '0';
    q.matmuld.vecaddr <= y;
    q.matmuld.resvecaddr <= w;
    
    q.usehintd.en <= '0';
    q.usehintd.nohint_writechash <= '1';
    q.usehintd.rst <= '1'; -- yes, this is correct.
    
    q.ballsampled.en <= '0';
    q.ballsampled.rst <= '0';
    
    q.chkzd.en <= '0';
    q.chkzd.ct0 <= '0';
    q.chkzd.rst <= '1'; -- yes, this is correct.

    nttd_sign.en <= '0';
    nttd_sign.inv <= '0';
    
    maccd_sign.en <= '0';
    maccd_sign.rst <= '0';
    maccd_sign.op <= op_macc;
    
    kappa_inc <= '0';
    kappa_rst <= '0';
    
    q.hregd.rst <= '0';
        
    case state is
        when idle =>
            q.ready <= '1';
            
            q.usehintd.rst <= '1';
            omegacntd.rst <= '1';
            shacntd.rst <= '1';
            kcntd.rst <= '1';
            lcntd.rst <= '1';
            memcntd.rst <= '1';
            keccakd_sign.rst <= '1';
            q.ballsampled.rst <= '1';
            q.chkzd.rst <= '1';
            kappa_rst <= '1';
            maccd_sign.rst <= '1';
            
            if d.en = '1' and d.keccakq.ready = '1'
            then
                keccakd_sign.en <= '1';
                keccakd_sign.rst <= '0';
                nextstate <= absorbK;
            end if;
            
        --------------------------------------------------------------------------------
        -- hash K and mu to rhoprime
        --------------------------------------------------------------------------------
        when absorbK =>
            keccakd_sign.en <= '1';
            q.Kregd.en_rotate <= '1';
            shacntd.en <= '1';
            
            if shacnt = 256/32-1
            then
                nextstate <= absorbmu;
            end if;
            
        when absorbmu =>
            keccakd_sign.en <= '1';
            muregd_sign.en_rotate <= '1';
            shacntd.en <= '1';
            
            if shacnt = (256 + 512)/32-1
            then
                nextstate <= padding;
            end if;
            
        when padding =>
            keccakd_sign.en <= '1';
            shacntd.en <= '1';
            
            if shacntq.max = '1'
            then
                nextstate <= zeropad;
            end if;
            
        when zeropad =>
            keccakd_sign.en <= '1';
            
            if d.keccakq.ready = '0'
            then
                nextstate <= permute;
            end if;
            
        when permute =>
            shacntd.rst <= '1';
            if d.keccakq.ready = '1'
            then
                nextstate <= squeeze;
            end if;
            
        when squeeze =>
            shacntd.en <= '1';
            keccakd_sign.en <= '1';
            rhoprimeregd_sign.en_rotate <= '1';
            rhoprimeregd_sign.en_write <= '1';
            
            if shacnt = 512/32 - 1
            then
                keccakd_sign.rst <= '1';
                nextstate <= expand_y;
                report "expandy start";
            end if;
        
        --------------------------------------------------------------------------------
        -- expand y
        --------------------------------------------------------------------------------
        when expand_y =>
            q.expandyd.en <= '1';
            
            if d.expandyq.ready = '0'
            then
                nextstate <= expand_y_finish;
            end if;
        
        when expand_y_finish =>
            q.expandyd.en <= '1';
            
            if d.expandyq.ready = '1'
            then
                nextstate <= matmul;
                report "expandy stop";
                report "matmul start";
            end if;
        
        --------------------------------------------------------------------------------
        -- matmul: w := Ay
        --------------------------------------------------------------------------------
        when matmul =>
            q.matmuld.en <= '1';
            
            if d.matmulq.ready = '0'
            then
                nextstate <= matmul_finish;
            end if;
        
        when matmul_finish =>
            q.matmuld.en <= '0';
            
            if d.matmulq.ready = '1'
            then
                kcntd.rst <= '1';
                nextstate <= inttw;
                report "matmul stop";
            end if;
            
        --------------------------------------------------------------------------------
        -- intt of w
        --------------------------------------------------------------------------------
        when inttw =>
            nttd_sign.en <= '1';
            nttd_sign.inv <= '1';
            
            if d.nttq.ready_read = '0'
            then
                nextstate <= inttw_finish;
            end if;
        
        when inttw_finish =>
            nttd_sign.en <= '0';
            nttd_sign.inv <= '1';
            if d.nttq.ready_read = '1'
            then
                kcntd.en <= '1';
                nextstate <= inttw_shortcut;
            end if;
        
        when inttw_shortcut =>
            if kcntq.ovf = '0'
            then
                nttd_sign.en <= '1';
            end if;
            nttd_sign.inv <= '1';
            if d.nttq.ready = '1'
            then
                if kcntq.ovf = '1'
                then
                    kcntd.rst <= '1';
                    nextstate <= highbits;
                    report "highbits start";
                else
                    nextstate <= inttw_finish;
                end if;
            end if;
        
        --------------------------------------------------------------------------------
        -- highbits
        --------------------------------------------------------------------------------
        when highbits =>
            q.usehintd.en <= '1';
            q.usehintd.rst <= '0';
            
            if d.usehintq.ready = '0'
            then
                nextstate <= highbits_finish;
            end if;
        
        when highbits_finish =>
            q.usehintd.rst <= '0';
            if d.usehintq.ready = '1'
            then
                nextstate <= ballsample;
                report "highbits stop";
                report "ballsample start";
            end if;
        
        --------------------------------------------------------------------------------
        -- ballsample
        --------------------------------------------------------------------------------
        when ballsample =>
            q.ballsampled.en <= '1';
            
            if d.ballsampleq.ready = '0'
            then
                nextstate <= ballsample_finish;
            end if;
        
        when ballsample_finish =>
            if d.ballsampleq.ready = '1'
            then
                report "ballsample stop";
                nextstate <= nttc;
            end if;
        
        --------------------------------------------------------------------------------
        -- ntt c
        --------------------------------------------------------------------------------
        when nttc =>
            nttd_sign.en <= '1';
            nttd_sign.inv <= '0';
            
            if d.nttq.ready = '0'
            then
                nextstate <= nttc_finish;
            end if;
        
        when nttc_finish =>
            if d.nttq.ready = '1'
            then
                nextstate <= y_cs1;
            end if;
        
        --------------------------------------------------------------------------------
        -- z := y+c*s1
        --------------------------------------------------------------------------------
        when y_cs1 =>
            maccd_sign.en <= '1';
            
            if d.maccq.ready_read = '0'
            then
                nextstate <= y_cs1_finish;
            end if;
        
        when y_cs1_finish => 
            if lcntq.max = '1' and d.maccq.ready = '1'
            then
                lcntd.rst <= '1';
                nextstate <= inttz;
            elsif lcntq.max = '0' and d.maccq.ready_read = '1'
            then
                lcntd.en <= '1';
                nextstate <= y_cs1_shortcut;
            end if;
        
        when y_cs1_shortcut =>
            maccd_sign.en <= '1';
            if d.maccq.ready = '1'
            then
                nextstate <= y_cs1_finish;
            end if;
        
        --------------------------------------------------------------------------------
        -- intt z
        --------------------------------------------------------------------------------
        when inttz =>
            nttd_sign.en <= '1';
            nttd_sign.inv <= '1';
            
            if d.nttq.ready_read = '0'
            then
                nextstate <= inttz_finish;
            end if;
        
        when inttz_finish => 
            nttd_sign.inv <= '1';
            if d.nttq.ready_read = '1'
            then
                lcntd.en <= '1';
                nextstate <= inttz_shortcut;
            end if;
        
        when inttz_shortcut =>
            if lcntq.ovf = '0'
            then
                nttd_sign.en <= '1';
            end if;
            nttd_sign.inv <= '1';
            if d.nttq.ready = '1'
            then
                if lcntq.ovf = '1'
                then
                    lcntd.rst <= '1';
                    nextstate <= check_z;
                else
                    nextstate <= inttz_finish;
                end if;
            end if;
        
        --------------------------------------------------------------------------------
        -- check z
        --------------------------------------------------------------------------------
        when check_z =>
            q.chkzd.en <= '1';
            q.chkzd.rst <= '0';
            q.chkzd.ct0 <= '0';
            if d.chkzq.ready = '0'
            then
                nextstate <= check_z_finish;
            end if;
            
        when check_z_finish =>
            q.chkzd.en <= '0';
            q.chkzd.rst <= '0';
            q.chkzd.ct0 <= '0';
            if d.chkzq.valid = '1'
            then
                if d.chkzq.result_bad = '1'
                then
                    report "restart z";
                    nextstate <= restart;
                else
                    nextstate <= cs2;
                end if;
            end if;
        
        --------------------------------------------------------------------------------
        -- tmp0 := c*s2
        --------------------------------------------------------------------------------
        when cs2 =>
            maccd_sign.en <= '1';
            
            if d.maccq.ready_read = '0'
            then
                nextstate <= cs2_finish;
            end if;
        
        when cs2_finish => 
            if kcntq.max = '1' and d.maccq.ready = '1'
            then
                kcntd.rst <= '1';
                nextstate <= inttcs2;
            elsif kcntq.max = '0' and d.maccq.ready_read = '1'
            then
                kcntd.en <= '1';
                nextstate <= cs2_shortcut;
            end if;
        
        when cs2_shortcut =>
            maccd_sign.en <= '1';
            if d.maccq.ready = '1'
            then
                nextstate <= cs2_finish;
            end if;
        
        --------------------------------------------------------------------------------
        -- intt tmp0 (cs2)
        --------------------------------------------------------------------------------
        when inttcs2 =>
            nttd_sign.en <= '1';
            nttd_sign.inv <= '1';
            
            if d.nttq.ready_read = '0'
            then
                nextstate <= inttcs2_finish;
            end if;
        
        when inttcs2_finish => 
            nttd_sign.inv <= '1';
            if d.nttq.ready_read = '1'
            then
                kcntd.en <= '1';
                nextstate <= inttcs2_shortcut;
            end if;
        
        when inttcs2_shortcut =>
            if kcntq.ovf = '0'
            then
                nttd_sign.en <= '1';
            end if;
            nttd_sign.inv <= '1';
            if d.nttq.ready = '1'
            then
                if kcntq.ovf = '1'
                then
                    kcntd.rst <= '1';
                    nextstate <= sub_from_w;
                else
                    nextstate <= inttcs2_finish;
                end if;
            end if;
        
        --------------------------------------------------------------------------------
        -- tmp0 := w-tmp0 = w-cs2
        --------------------------------------------------------------------------------
        when sub_from_w =>
            maccd_sign.en <= '1';
            maccd_sign.op <= op_sub;
            
            if d.maccq.ready_read = '0'
            then
                nextstate <= sub_from_w_finish;
            end if;
        
        when sub_from_w_finish => 
            maccd_sign.op <= op_sub;
            if kcntq.max = '1' and d.maccq.ready = '1'
            then
                kcntd.rst <= '1';
                nextstate <= lowbits;
            elsif kcntq.max = '0' and d.maccq.ready_read = '1'
            then
                kcntd.en <= '1';
                nextstate <= sub_from_w_shortcut;
            end if;
        
        when sub_from_w_shortcut =>
            maccd_sign.en <= '1';
            maccd_sign.op <= op_sub;
            if d.maccq.ready = '1'
            then
                nextstate <= sub_from_w_finish;
            end if;
        
        --------------------------------------------------------------------------------
        -- check low bits
        --------------------------------------------------------------------------------
        when lowbits =>
            maccd_sign.en <= '1';
            maccd_sign.op <= op_sub;
            
            if d.maccq.ready_read = '0'
            then
                nextstate <= lowbits_finish;
            end if;
            if lowbits_valid = '0'
            then
                report "restart lowbits";
                nextstate <= restart;
            end if;
        
        when lowbits_finish => 
            maccd_sign.op <= op_sub;
            if kcntq.max = '1' and d.maccq.ready = '1'
            then
                kcntd.rst <= '1';
                nextstate <= ct0;
            elsif kcntq.max = '0' and d.maccq.ready_read = '1'
            then
                kcntd.en <= '1';
                nextstate <= lowbits_shortcut;
            end if;
            if lowbits_valid = '0'
            then
                report "restart lowbits";
                nextstate <= restart;
            end if;
        
        when lowbits_shortcut =>
            maccd_sign.en <= '1';
            maccd_sign.op <= op_sub;
            if d.maccq.ready = '1'
            then
                nextstate <= lowbits_finish;
            end if;
            if lowbits_valid = '0'
            then
                report "restart lowbits";
                nextstate <= restart;
            end if;
        
        --------------------------------------------------------------------------------
        -- tmp1 := ct0
        --------------------------------------------------------------------------------
        when ct0 =>
            maccd_sign.en <= '1';
            maccd_sign.op <= op_macc;
            
            if d.maccq.ready_read = '0'
            then
                nextstate <= ct0_finish;
            end if;
        
        when ct0_finish => 
            maccd_sign.op <= op_macc;
            if kcntq.max = '1' and d.maccq.ready = '1'
            then
                kcntd.rst <= '1';
                nextstate <= inttct0;
            elsif kcntq.max = '0' and d.maccq.ready_read = '1'
            then
                kcntd.en <= '1';
                nextstate <= ct0_shortcut;
            end if;
        
        when ct0_shortcut =>
            maccd_sign.en <= '1';
            maccd_sign.op <= op_macc;
            if d.maccq.ready = '1'
            then
                nextstate <= ct0_finish;
            end if;
        
        --------------------------------------------------------------------------------
        -- tmp1 := intt(ct0)
        --------------------------------------------------------------------------------
        when inttct0 =>
            nttd_sign.en <= '1';
            nttd_sign.inv <= '1';
            
            if d.nttq.ready_read = '0'
            then
                nextstate <= inttct0_finish;
            end if;
        
        when inttct0_finish => 
            nttd_sign.inv <= '1';
            
            if d.nttq.ready_read = '1'
            then
                kcntd.en <= '1';
                nextstate <= inttct0_shortcut;
            end if;
        
        when inttct0_shortcut =>
            if kcntq.ovf = '0'
            then
                nttd_sign.en <= '1';
            end if;
            nttd_sign.inv <= '1';
            if d.nttq.ready = '1'
            then
                if kcntq.ovf = '1'
                then
                    kcntd.rst <= '1';
                    nextstate <= check_ct0;
                else
                    nextstate <= inttct0_finish;
                end if;
            end if;
        
        --------------------------------------------------------------------------------
        -- check ct0 (tmp1)
        --------------------------------------------------------------------------------
        when check_ct0 =>
            q.chkzd.en <= '1';
            q.chkzd.rst <= '0';
            q.chkzd.ct0 <= '1';
            
            if d.chkzq.ready = '0'
            then
                nextstate <= check_ct0_finish;
            end if;
            
        when check_ct0_finish =>
            q.chkzd.en <= '0';
            q.chkzd.rst <= '0';
            q.chkzd.ct0 <= '1';
            
            if d.chkzq.valid = '1'
            then
                if d.chkzq.result_bad = '1'
                then
                    report "restart ct0";
                    nextstate <= restart;
                else
                    nextstate <= add_wcs2;
                end if;
            end if;
            
        --------------------------------------------------------------------------------
        -- tmp1 := tmp1 + tmp0 = w-cs2+ct0
        --------------------------------------------------------------------------------
        when add_wcs2 =>
            maccd_sign.en <= '1';
            maccd_sign.op <= op_add;
            
            if d.maccq.ready_read = '0'
            then
                nextstate <= add_wcs2_finish;
            end if;
        
        when add_wcs2_finish => 
            maccd_sign.op <= op_add;
            if kcntq.max = '1' and d.maccq.ready = '1'
            then
                kcntd.rst <= '1';
                memcntd.rst <= '1';
                q.hregd.rst <= '1';
                nextstate <= makehint;
                report "makehint start";
            elsif kcntq.max = '0' and d.maccq.ready_read = '1'
            then
                kcntd.en <= '1';
                nextstate <= add_wcs2_shortcut;
            end if;
        
        when add_wcs2_shortcut =>
            maccd_sign.en <= '1';
            maccd_sign.op <= op_add;
            if d.maccq.ready = '1'
            then
                nextstate <= add_wcs2_finish;
            end if;
        
        --------------------------------------------------------------------------------
        -- make hint from tmp0 and tmp1
        --------------------------------------------------------------------------------
        when makehint =>
            memcntd.en <= '1';
            
            if memcntq.max = '1'
            then
                if omegacntq.max = '1'
                then
                    report "makehint stop";
                    nextstate <= idle;
                else
                    nextstate <= makehint_flush;
                end if;
            end if;
            if omegacntq.ovf = '1'
            then
                report "makehint break";
                nextstate <= restart;
            end if;
            
        when makehint_flush =>
            if omegacntq.max = '1'
            then
                report "makehint stop";
                nextstate <= idle;
            end if;
        
        --------------------------------------------------------------------------------
        -- abort, restart
        --------------------------------------------------------------------------------
        when restart =>
            report "expandy start";
            keccakd_sign.rst <= '1';
            omegacntd.rst <= '1';
            maccd_sign.rst <= '1';
            kappa_inc <= '1';
            nextstate <= expand_y;
        
        when others => nextstate <= idle;
        
    end case;
end process;

end Behavioral;
