;
; Abstract class for all disassembler output.
;

(require 'cl)
(provide 'z80-dis-output)

(defstruct z80-dis-output
  start-address
  end-address
  start
  string
  int
  address
  disp
  end)
