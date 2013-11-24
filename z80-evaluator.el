;
; Z80-EVALUATOR
;

(require 'cl)
(provide 'z80-evaluator)


; The interface for any form of evaluator for a Z80.
; Given the lack of static typing, and the likleyhood of
; comments getting out of date, the only way to work out
; what arguments each of the functions expects is to look
; at an existing evaluator e.g. z80-dis.el

(defstruct z80-evaluator
  illegal

  nop           ld-bc-imm     ld-bc-a      inc-bc
  inc-b         dec-b         ld-b-imm     rcla

  ex-af         add-hl-bc     ld-a-bc      dec-bc
  inc-c         dec-c         ld-c-imm     rrca

  djnz          ld-de-imm     ld-de-a      inc-de
  inc-d         dec-d         ld-d-imm     rla

  jr            add-hl-de     ld-a-de      dec-de
  inc-e         dec-e         ld-e-imm     rra

  jr-nz         ld-hl-imm     st-hl        inc-hl
  inc-h         dec-h         ld-h-imm     daa

  jr-z          add-hl-hl     ld-hl-addr   dec-hl
  inc-l         dec-l         ld-l-imm     cpl

  jr-nc         ld-sp-imm     st-a         inc-sp
  inc-hl-addr   dec-hl-addr   st-hl-imm    scf

  jr-c          add-hl-sp     ld-a-addr    dec-sp
  inc-a         dec-a         ld-a-imm     ccf

  ld-b-reg      ld-b-hl       ld-c-reg     ld-c-hl
  ld-d-reg      ld-d-hl       ld-e-reg     ld-e-hl 
  ld-h-reg      ld-h-hl       ld-l-reg     ld-l-hl
  st-hl-reg     halt          ld-a-reg     ld-a-hl
  add-a-reg     add-a-hl      adc-a-reg    adc-a-hl
  sub-a-reg     sub-a-hl      sbc-a-reg    sbc-a-hl
  and-reg       and-hl        xor-reg      xor-hl
  or-reg        or-hl         cp-reg       cp-hl

  ret-nz        pop-bc        jp-nz        jp
  call-nz       push-bc       add-a-imm    rst-00

  ret-z         ret           jp-z 

  rlc-reg       rlc-hl        rrc-reg      rrc-hl
  rl-reg        rl-hl         rr-reg       rr-hl
  sla-reg       sla-hl        sra-reg      sra-hl
  srl-reg       srl-hl        bit-reg      bit-hl
  res-reg       res-hl        set-reg      set-hl

  call-z        call          adc-imm      rst-08
  ret-nc        pop-de        jp-nc        out-a

  call-nc       push-de       sub-imm      rst-10
  ret-c         exx           jp-c         in-a

  call-c

  add-ix        ld-ix-imm     st-ix        inc-ix
  ld-ix-addr    dec-ix        inc-ix-disp  dec-ix-disp
  ; other DD codes here
  add-a-ix-disp adc-a-ix-disp sub-ix-disp  sbc-ix-disp
  and-ix-disp   xor-ix-disp   or-ix-disp   cp-ix-disp
  ; rest of DD codes here
  pop-ix          ex-sp-ix        push-ix        jp-ix
  ld-sp-ix

  sbc-imm       rst-18        ret-po       pop-hl
  jp-po         ex-sp-hl      call-po      push-hl

  and-imm       rst-20        ret-pe       jp-hl
  jp-pe         ex-de-hl      call-pe

  ; other ed stuff goes here
  neg           retn          ld-i-a       reti
  ld-r-a        ld-a-i        ld-a-r       rrd
  rld           ldi           cpi          ini
  outi          ldd           cpd          ind
  outd          ldir          cpir         inir
  otir          lddr          cpdr         indr
  otdr

  xor-imm       rst-28        ret-p        pop-af
  jp-p          di            call-p       push-af

  or-imm          rst-30          ret-m          ld-sp-hl
  jp-m            ei              call-m         add-iy              
  ld-iy-imm       st-iy           inc-iy         ld-iy-addr
  dec-iy          inc-iy-disp     dec-iy-disp    st-iy-disp-imm
  ld-reg-iy-disp  st-iy-disp-reg  
  add-a-iy-disp   adc-a-iy-disp   sub-iy-disp    sbc-iy-disp
  and-iy-disp     xor-iy-disp     or-iy-disp     cp-iy-disp
  ; more FD codes here.
  pop-iy          ex-sp-iy        push-iy        jp-iy
  ld-sp-iy

  cp-imm        rst-38)
