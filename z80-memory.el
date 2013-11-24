;
; Z80-MEMORY and Z80-ADDRESS
;

(require 'cl)
(provide 'z80-memory)

; Addresses

(defconst z80-memory-max-size 65535)

(defun z80-address-next (address)
  "Return the next address after ADDRESS"
  (mod (+ address 1) z80-memory-max-size))

(defun z80-address-delta (address d)
  "Return the ADDRESS ..."
  (mod (+ address d) z80-memory-max-size))


; Memory

(defstruct z80-memory  read-byte close)

(defun z80-memory-word (m addr)
  "Extract a 16-bit integer from the Z80-MEMORY at the given ADDRESS"
  (let ((get-byte (z80-memory-read-byte m))
        (next-addr (z80-address-next addr)))
    (let ((lo (funcall get-byte m addr))
          (hi (funcall get-byte m next-addr)))
      (logior (lsh hi 8) lo))))
