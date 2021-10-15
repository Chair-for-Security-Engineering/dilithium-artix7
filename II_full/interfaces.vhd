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
use work.memmap.all;

package interfaces is

------------------------
-- mem_8_quarter_poly --
------------------------
    type mem_8_quarter_poly_in_type is record
        ren : std_logic;
        raddr : std_logic_vector(8 downto 0);
        wen : std_logic;
        waddr : std_logic_vector(8 downto 0);
        data : std_logic_vector(22 downto 0);
    end record mem_8_quarter_poly_in_type;
    
----------------
-- mem_8_poly --
----------------
    type mem_8_poly_in_type is record
        rsel  : natural range 0 to 7;
        ren  : std_logic_vector(0 to 3);
        raddr : coef_addr_array(0 to 3);
        
        wsel  : natural range 0 to 7;
        wen  : std_logic_vector(0 to 3);
        waddr : coef_addr_array(0 to 3);
        wdata : payload_array(0 to 3);
    end record mem_8_poly_in_type;
    
------------
-- memory --
------------
    type memory_in_type is array(0 to NUM_MEM_8_POLY-1) of mem_8_poly_in_type;
    
    type memory_out_type is array(0 to NUM_MEM_8_POLY-1) of payload_array(0 to 3);
    
    constant ZEROMEM : mem_8_poly_in_type := (rsel => 0, ren => "0000", raddr => (others => (others => '0')), wsel => 0, wen => "0000", waddr => (others => (others => '0')), wdata => (others => (others => '0')));
    constant GLOBAL_MEMORY_DELAY : natural := 5;



-----------
-- reg32 --
-----------
    type reg32_in_type is record
        en_rotate, en_write : std_logic;
        data : std_logic_vector(31 downto 0);
    end record reg32_in_type;
    constant ZEROREG32 : reg32_in_type := (en_rotate => '0', en_write => '0', data => (others => '0'));
    
    type reg32_out_type is record
        data : std_logic_vector(31 downto 0);
    end record reg32_out_type;

----------
-- fifo --
----------
    type fifo_in_type is record
        en, rst : std_logic;
        valid : std_logic;
        ready_rcv : std_logic;
    end record fifo_in_type;
    
    type fifo_out_type is record
        ready : std_logic;
        ready_rcv : std_logic;
        valid : std_logic;
        toggle : std_logic;
    end record fifo_out_type;
    
----------
-- hreg --
----------
    type hreg_in_type is record
        en_rotate_data, en_write_data : std_logic;
        en_rotate_poly, en_write_poly : std_logic;
        data_offset : std_logic_vector(7 downto 0);
        poly_offset : std_logic_vector(7 downto 0);
    end record hreg_in_type;
    constant ZEROHREG : hreg_in_type := (data_offset => (others => '0'), poly_offset => (others => '0'), others => '0');
    
    type hreg_out_type is record
        data_offset : std_logic_vector(7 downto 0);
        poly_offset : std_logic_vector(7 downto 0);
    end record hreg_out_type;
    
----------------
-- convert_yz --
----------------
    constant DELAY_CONV_YZ : natural := 3;
    type convert_yz_in_type is record
        en : std_logic;
        sub : std_logic_vector(22 downto 0);
        data : payload_array(0 to 3);
    end record convert_yz_in_type;
    constant ZEROCONVYZ : convert_yz_in_type := (en => '0', sub => (others => '0'), data => (others => (others => '0')));
    
-------------
-- counter --
-------------
    type counter_in_type is record
        en, rst : std_logic;
    end record counter_in_type;
    
    type counter_out_type is record
        ovf, max : std_logic;
    end record counter_out_type;

------------
-- keccak --
------------
    type keccak_in_type is record
        en : std_logic;
        rst : std_logic;
        data : std_logic_vector(31 downto 0);
    end record keccak_in_type;
    constant ZEROKECCAK : keccak_in_type := (data => (others => '0'), others => '0');
    
    type keccak_out_type is record
        ready : std_logic;
        data : std_logic_vector(31 downto 0);
    end record keccak_out_type;

------------
-- expand --
------------
    type expand_in_type is record
        -- ctrl in
        en     : std_logic;
        -- data in
        seedregq : reg32_out_type;
        -- interface
        keccakq : keccak_out_type;
    end record expand_in_type;

    type expand_out_type is record
        ready  : std_logic;
        keccakd : keccak_in_type;
        memd   : memory_in_type;
        seedregd : reg32_in_type;
    end record expand_out_type;

--------------
-- expand_y --
--------------
    type expand_y_in_type is record
        en     : std_logic;
        seedregq : reg32_out_type;
        kappa  : natural range 0 to 2**16-1;
        keccakq : keccak_out_type;
        convertyzq : payload_array(0 to 3);
        fifoyzq : fifo_out_type;
        fifoyzdataq : std_logic_vector((DILITHIUM_loggamma1+1)*4-1 downto 0);
    end record expand_y_in_type;

    type expand_y_out_type is record
        ready  : std_logic;
        seedregd : reg32_in_type;
        keccakd : keccak_in_type;
        memd   : memory_in_type;
        convertyzd : convert_yz_in_type;
        fifoyzd : fifo_in_type;
        fifoyzdatad : std_logic_vector(31 downto 0);
    end record expand_y_out_type;

--------------
-- highbits --
--------------
    type highbits_in_type is array(0 to 3) of std_logic_vector(22 downto 0);
    type highbits_out_type is record
        high : payload_array(0 to 3);
        lowsign : std_logic_vector(0 to 3);
    end record;
    
--------------
-- use_hint --
--------------
    type use_hint_in_type is record
        en : std_logic;
        rst : std_logic;
        nohint_writechash : std_logic;
        
        keccakq : keccak_out_type;
        muregq : reg32_out_type;
        chashregq : reg32_out_type;
        hregq : hreg_out_type;
        memq : memory_out_type;
    end record use_hint_in_type;
    type use_hint_out_type is record
        ready : std_logic;
        valid : std_logic;
        result : std_logic;
        
        keccakd : keccak_in_type;
        muregd : reg32_in_type;
        chashregd : reg32_in_type;
        hregd : hreg_in_type;
        memd : memory_in_type;
    end record use_hint_out_type;

---------------
-- macc_poly --
---------------
    type macc_poly_op_type is (op_macc, op_add, op_sub);

    type macc_poly_in_type is record
        en,rst : std_logic;
        op : macc_poly_op_type;
        memq_a, memq_b, memq_c : payload_array(0 to 3);
    end record macc_poly_in_type;
    
    type macc_poly_out_type is record
        ready : std_logic;
        ready_read : std_logic;
        ren, wen : std_logic;
        raddr, waddr : coef_addr_array(0 to 3);
        wdata : payload_array(0 to 3);
    end record macc_poly_out_type;

----------------
-- macc_coeff --
----------------
    type macc_coeff_in_type is record
        en : std_logic;
        op : macc_poly_op_type;
        a : payload_array(0 to 3);
        b : payload_array(0 to 3);
        c : payload_array(0 to 3);
    end record macc_coeff_in_type;
    
    type macc_coeff_out_type is array(0 to 3) of std_logic_vector(22 downto 0);

--------------
-- macc_dsp --
--------------
    type macc_dsp_in_type is record
        en : std_logic;
        op : macc_poly_op_type;
        a : std_logic_vector(22 downto 0);
        b : std_logic_vector(22 downto 0);
        c : std_logic_vector(22 downto 0);
    end record macc_dsp_in_type;

-----------------
-- mem_twiddle --
-----------------
    type mem_twiddle_addr_array is array(natural range <>) of std_logic_vector(8 downto 0);
    
    type mem_twiddle_in_type is record
        en   : std_logic_vector(0 to 1);
        addr : mem_twiddle_addr_array(0 to 1);
    end record mem_twiddle_in_type;
    
    type mem_twiddle_out_type is array(0 to 1) of std_logic_vector(22 downto 0);

---------
-- ntt --
---------
    type ntt_addr_gen_in_type is record
        en : std_logic;
        rst : std_logic;
        inv : std_logic;
    end record ntt_addr_gen_in_type;
    
    type ntt_addr_gen_out_type is record
        ready : std_logic;
        addr : coef_addr_array(0 to 3);
        twiddlectrl : mem_twiddle_in_type;
    end record ntt_addr_gen_out_type;

    type ntt_in_type is record
        en : std_logic;
        inv : std_logic;
        data : payload_array(0 to 3);
    end record ntt_in_type;
    
    type ntt_out_type is record
        ready : std_logic;
        ready_read : std_logic;
        ren : std_logic;
        wen : std_logic;
        raddr : coef_addr_array(0 to 3);
        waddr : coef_addr_array(0 to 3);
        wdata : payload_array(0 to 3);
    end record ntt_out_type;
    
-------------------
-- mul_red_23x23 --
-------------------
    type mul_red_23x23_in_type is record
        a : std_logic_vector(22 downto 0);
        b : std_logic_vector(22 downto 0);
    end record mul_red_23x23_in_type;
    
    type mul_red_23x23_out_type is record
        p : std_logic_vector(22 downto 0);
    end record mul_red_23x23_out_type;

---------------
-- Reduction --
---------------
    type red_dsp_in_type is record
        en : std_logic;
        data : std_logic_vector(45 downto 0);
    end record red_dsp_in_type;
        
    type red_dsp_out_type is record
        data :  std_logic_vector(22 downto 0);
    end record red_dsp_out_type;
    
    -- stage 1
    type red_dsp_1_in_type is record
        en : std_logic;
        a : std_logic_vector(23 downto 0);
        c : std_logic_vector(33 downto 0);
        d : std_logic_vector(13 downto 0);
        cin : std_logic;
    end record red_dsp_1_in_type;
    
    type red_dsp_1_out_type is record
        p : std_logic_vector(35 downto 0);
    end record red_dsp_1_out_type;
    
    -- stage 2
    type red_dsp_2_in_type is record
        en : std_logic;
        a : std_logic_vector(1 downto 0);
        c : std_logic_vector(23 downto 0);
        d : std_logic_vector(22 downto 0);
    end record red_dsp_2_in_type;
    
    type red_dsp_2_out_type is record
        p : std_logic_vector(26 downto 0);
        pcout : std_logic_vector(47 downto 0);
    end record red_dsp_2_out_type;
    
    -- stage 3
    type red_dsp_3_in_type is record
        en : std_logic;
        pcin : std_logic_vector(47 downto 0);
        e1 : std_logic_vector(9 downto 0);
        e2 : std_logic_vector(1 downto 0);
    end record red_dsp_3_in_type;
    
    type red_dsp_3_out_type is record
        p : std_logic_vector(47 downto 0);
        pcout : std_logic_vector(47 downto 0);
    end record red_dsp_3_out_type;
    
    -- stage 4
    type red_dsp_4_in_type is record
        en : std_logic;
        pcin : std_logic_vector(47 downto 0);
        c : std_logic_vector(23 downto 0);
    end record red_dsp_4_in_type;
    
    type red_dsp_4_out_type is record
        p : std_logic_vector(47 downto 0);
    end record red_dsp_4_out_type;

---------------------
-- Butterfly Units --
---------------------
    type double_bfu_in_type is record
        en : std_logic;
        inv : std_logic;
        a : payload_array(0 to 1);
        b : payload_array(0 to 1);
        omega : payload_array(0 to 1);
    end record double_bfu_in_type;
    
    type double_bfu_out_type is record
        A, B : payload_array(0 to 1);
    end record double_bfu_out_type;

    type butterfly_in_type is record
        en, inv : std_logic;
        a : std_logic_vector(22 downto 0);
        b : std_logic_vector(22 downto 0);
        a_minus_b : std_logic_vector(23 downto 0); -- signed!
        omega : std_logic_vector(22 downto 0);
    end record butterfly_in_type;
    
    type butterfly_out_type is record
        A : std_logic_vector(22 downto 0);
        B : std_logic_vector(22 downto 0);
    end record butterfly_out_type;
    
    -- stage 1
    type bfu_stage_1_in_type is record
        en, sel : std_logic;
        a : std_logic_vector(23 downto 0); -- signed
        b : std_logic_vector(16 downto 0); -- unsigned
        c : std_logic_vector(22 downto 0); -- unsigned
    end record bfu_stage_1_in_type;
    
    type bfu_stage_1_out_type is record
        acout : std_logic_vector(29 downto 0);
        pcout : std_logic_vector(47 downto 0);
        p : std_logic_vector(16 downto 0);
    end record bfu_stage_1_out_type;
    
    -- stage 2
    type bfu_stage_2_in_type is record
        en : std_logic;
        acin : std_logic_vector(29 downto 0);
        pcin : std_logic_vector(47 downto 0);
        b : std_logic_vector(5 downto 0);
        sign : std_logic;
    end record bfu_stage_2_in_type;
    
    type bfu_stage_2_out_type is record
        p : std_logic_vector(47 downto 0);
    end record bfu_stage_2_out_type;
    
    -- adder
    type bfu_adder_in_type is record
        en, sel : std_logic;
        a : std_logic_vector(22 downto 0);
        c : std_logic_vector(23 downto 0);
        d : std_logic_vector(23 downto 0);
    end record bfu_adder_in_type;
    
    type bfu_adder_out_type is record
        p : std_logic_vector(47 downto 0);
        pcout : std_logic_vector(47 downto 0);
    end record bfu_adder_out_type;
    
    -- reducer
    type bfu_reducer_in_type is record
        en, sel : std_logic;
        pcin : std_logic_vector(47 downto 0);
    end record bfu_reducer_in_type;
    
    type bfu_reducer_out_type is record
        p : std_logic_vector(47 downto 0);
    end record bfu_reducer_out_type;
    
    -- subtracter
    type bfu_subtracter_in_type is record
        en : std_logic;
        -- all unsigned
        a : payload_array(0 to 1);
        b : payload_array(0 to 1);
    end record bfu_subtracter_in_type;
    
    type bfu_subtracter_out_type is record
        p :  payload_array(0 to 1);
        sign : std_logic_vector(0 to 1);
    end record bfu_subtracter_out_type;

----------------
-- crh_rho_t1 --
----------------
    type crh_rho_t1_in_type is record
        en : std_logic;
        keccakq : keccak_out_type;
        rhoregq : reg32_out_type;
        memq : memory_out_type;
    end record crh_rho_t1_in_type;
    
    type crh_rho_t1_out_type is record
        ready : std_logic;
        rhoregd : reg32_in_type;
        keccakd : keccak_in_type;
        memd : memory_in_type;
        trregd : reg32_in_type;
    end record crh_rho_t1_out_type;

------------
-- matmul --
------------
    type matmul_in_type is record
        en : std_logic;
        vecaddr : matvecaddr_type;
        resvecaddr : matvecaddr_type;
        
        -- interfaces
        memq : memory_out_type;
        nttq : ntt_out_type;
        maccq : macc_poly_out_type;
    end record matmul_in_type;
    
    type matmul_out_type is record
        ready : std_logic;
        
        -- interfaces
        memd : memory_in_type;
        nttd : ntt_in_type;
        maccd : macc_poly_in_type;
    end record matmul_out_type;

-------------
-- check_z --
-------------
    type check_z_in_type is record
        en,rst : std_logic;
        ct0 : std_logic;
        memq : memory_out_type;
    end record check_z_in_type;
    
    type check_z_out_type is record
        ready, valid, result_bad : std_logic;
        memd : memory_in_type;
    end record check_z_out_type;

----------------
-- ballsample --
----------------
    type ballsample_in_type is record
        en, rst : std_logic;
        
        keccakq : keccak_out_type;
        seedregq : reg32_out_type;
    end record ballsample_in_type;
    
    type ballsample_out_type is record
        ready : std_logic;
        
        keccakd : keccak_in_type;
        seedregd : reg32_in_type;
        memd : memory_in_type;
    end record ballsample_out_type;
    





------------
-- keygen --
------------
    type keygen_in_type is record
        -- ctrl input
        en : std_logic;
        
        -- data input
        seedregq : reg32_out_type;
        
        -- interfaces
        keccakq : keccak_out_type;
        memq : memory_out_type;
        maccq : macc_poly_out_type;
        nttq : ntt_out_type;
        expandAq : expand_out_type;
        expands1s2q : expand_out_type;
        matmulq : matmul_out_type;
        crtq : crh_rho_t1_out_type;
        convyzq : payload_array(0 to 3);
        rhoregq, rhoprimeregq, Kregq, trregq : reg32_out_type;
    end record keygen_in_type;
    
    type keygen_out_type is record
        -- ctrl output
        ready : std_logic;
        
        -- data in ctrl
        seedregd : reg32_in_type;
        
        -- interfaces
        keccakd : keccak_in_type;
        memd : memory_in_type;
        maccd : macc_poly_in_type;
        nttd : ntt_in_type;
        expandAd : expand_in_type;
        expands1s2d : expand_in_type;
        matmuld : matmul_in_type;
        crtd : crh_rho_t1_in_type;
        convyzd : convert_yz_in_type;
        rhoregd, rhoprimeregd, Kregd, trregd : reg32_in_type;
    end record keygen_out_type;
    
------------------
-- sign_precomp --
------------------
    type sign_precomp_in_type is record
        -- ctrl input
        en : std_logic;
        
        -- interfaces
        keccakq : keccak_out_type;
        memq : memory_out_type;
        nttq : ntt_out_type;
        expandAq : expand_out_type;
        rhoregq : reg32_out_type;
    end record sign_precomp_in_type;
    
    type sign_precomp_out_type is record
        -- ctrl output
        ready : std_logic;
        
        -- interfaces
        keccakd : keccak_in_type;
        memd : memory_in_type;
        nttd : ntt_in_type;
        expandAd : expand_in_type;
        rhoregd : reg32_in_type;
    end record sign_precomp_out_type;
    
----------
-- sign --
----------
    type sign_in_type is record
        -- ctrl input
        en : std_logic;
        
        -- interfaces
        convyzq : payload_array(0 to 3);
        usehintq : use_hint_out_type;
        keccakq : keccak_out_type;
        matmulq : matmul_out_type;
        memq : memory_out_type;
        nttq : ntt_out_type;
        maccq : macc_poly_out_type;
        expandyq : expand_y_out_type;
        ballsampleq : ballsample_out_type;
        chkzq : check_z_out_type;
        Kregq : reg32_out_type;
        muregq : reg32_out_type;
        rhoregq : reg32_out_type;
        rhoprimeregq : reg32_out_type;
        chashregq : reg32_out_type;
        hregq : hreg_out_type;
        fifoyzq : fifo_out_type;
        fifoyzdataq : std_logic_vector((DILITHIUM_loggamma1+1)*4-1 downto 0);
    end record sign_in_type;
    
    type sign_out_type is record
        -- ctrl output
        ready : std_logic;
        
        -- interfaces
        convyzd : convert_yz_in_type;
        usehintd : use_hint_in_type;
        keccakd : keccak_in_type;
        matmuld : matmul_in_type;
        memd : memory_in_type;
        ballsampled : ballsample_in_type;
        chkzd : check_z_in_type;
        nttd : ntt_in_type;
        maccd : macc_poly_in_type;
        expandyd : expand_y_in_type;
        Kregd : reg32_in_type;
        muregd : reg32_in_type;
        rhoregd : reg32_in_type;
        rhoprimeregd : reg32_in_type;
        chashregd : reg32_in_type;
        hregd : hreg_in_type;
        fifoyzd : fifo_in_type;
        fifoyzdatad : std_logic_vector(31 downto 0);
    end record sign_out_type;
    
--------------------
-- verify_precomp --
--------------------
    type verify_precomp_in_type is record
        -- ctrl input
        en : std_logic;
        
        -- interfaces
        keccakq : keccak_out_type;
        memq : memory_out_type;
        nttq : ntt_out_type;
        expandAq : expand_out_type;
        crt1q : crh_rho_t1_out_type;
        rhoregq : reg32_out_type;
    end record verify_precomp_in_type;
    
    type verify_precomp_out_type is record
        -- ctrl output
        ready : std_logic;
        
        -- interfaces
        keccakd : keccak_in_type;
        memd : memory_in_type;
        nttd : ntt_in_type;
        expandAd : expand_in_type;
        crt1d : crh_rho_t1_in_type;
        rhoregd : reg32_in_type;
        trregd : reg32_in_type;
    end record verify_precomp_out_type;
    
------------
-- verify --
------------
    type verify_in_type is record
        -- ctrl input
        en : std_logic;
        rst : std_logic;
        
        -- interfaces
        usehintq : use_hint_out_type;
        ballsampleq : ballsample_out_type;
        chkzq : check_z_out_type;
        matmulq : matmul_out_type;
        nttq : ntt_out_type;
        maccq : macc_poly_out_type;
        keccakq : keccak_out_type;
        memq : memory_out_type;
        hregq : hreg_out_type;
        muregq : reg32_out_type;
        chashregq : reg32_out_type;
    end record verify_in_type;
    
    type verify_out_type is record
        -- ctrl output
        ready : std_logic;
        valid : std_logic;
        
        -- data out
        result : std_logic;
        
        -- interfaces
        usehintd : use_hint_in_type;
        ballsampled : ballsample_in_type;
        chkzd : check_z_in_type;
        matmuld : matmul_in_type;
        nttd : ntt_in_type;
        maccd : macc_poly_in_type;
        keccakd : keccak_in_type;
        memd : memory_in_type;
        hregd : hreg_in_type;
        muregd : reg32_in_type;
        chashregd : reg32_in_type;
    end record verify_out_type;
    
----------------
-- digest_msg --
----------------
    type digest_msg_in_type is record
        -- ctrl input
        en : std_logic;
        valid : std_logic;
        done : std_logic;
        
        payload : std_logic_vector(31 downto 0);
        
        -- interfaces
        keccakq : keccak_out_type;
        trregq : reg32_out_type;
    end record digest_msg_in_type;
    
    type digest_msg_out_type is record
        -- ctrl output
        ready : std_logic;
        ready_rcv : std_logic;
        
        -- interfaces
        keccakd : keccak_in_type;
        trregd : reg32_in_type;
        muregd : reg32_in_type;
    end record digest_msg_out_type;
    
----------
-- load --
----------
    type load_in_type is record
        en : std_logic;
        valid : std_logic;
        ready_rcv : std_logic;
        payload_type : std_logic_vector(1 downto 0); -- pk/sk/signature
        hregq : hreg_out_type;
        memq : memory_out_type;
        convyzq : payload_array(0 to 3);
        rhoregq : reg32_out_type;
        Kregq : reg32_out_type;
        trregq : reg32_out_type;
        chashregq : reg32_out_type;
    end record load_in_type;
    
    type load_out_type is record
        ready : std_logic;
        valid : std_logic;
        payload : std_logic_vector(31 downto 0);
        hregd : hreg_in_type;
        memd : memory_in_type;
        convyzd : convert_yz_in_type;
        rhoregd : reg32_in_type;
        Kregd : reg32_in_type;
        trregd : reg32_in_type;
        chashregd : reg32_in_type;
    end record load_out_type;
    
-----------
-- store --
-----------
    type store_in_type is record
        en, rst : std_logic;
        valid : std_logic;
        payload_type : std_logic_vector(1 downto 0); -- pk/sk/signature/keygen seed
        payload : std_logic_vector(31 downto 0);
        
        convyzq : payload_array(0 to 3);
        zfifoq : fifo_out_type;
        zfifodataq : std_logic_vector((DILITHIUM_loggamma1+1)*4-1 downto 0);
        fifo160q : fifo_out_type;
        fifo160dataq : std_logic_vector(39 downto 0);
        fifot0q : fifo_out_type;
        fifot0dataq : std_logic_vector(51 downto 0);
    end record store_in_type;
    
    type store_out_type is record
        ready_rcv : std_logic;
        ready : std_logic;
        
        memd : memory_in_type;
        rhoregd : reg32_in_type;
        Kregd : reg32_in_type;
        trregd : reg32_in_type;
        chashregd : reg32_in_type;
        seedregd : reg32_in_type;
        hregd : hreg_in_type;
        convyzd : convert_yz_in_type;
        zfifod : fifo_in_type;
        zfifodatad : std_logic_vector(31 downto 0);
        fifo160d : fifo_in_type;
        fifo160datad : std_logic_vector(31 downto 0);
        fifot0d : fifo_in_type;
        fifot0datad : std_logic_vector(31 downto 0);
    end record store_out_type;

end interfaces;
