;
; Buffer Memory
;

(require 'cl)
(require 'z80-memory)
(provide 'z80-memory-buffer)


(defstruct (z80-memory-buffer (:include z80-memory))  buffer)


; This implementation works ok for a read-only evaluator like
; a disassembler, but to work with a read/write evaluator like
; an executor, some sort of caching will be needed.

(defun z80-memory-buffer-byte (memory address)
  "Private function which returns the byte in the MEMORY at the given ADDRESS"
  (save-excursion
    (set-buffer (z80-memory-buffer-buffer memory))
    (char-after address)))


(defun z80-memory-buffer-from-buffer (buffer)
  "Make a Z80-MEMORY out of a BUFFER"
  (make-z80-memory-buffer :read-byte z80-memory-buffer-byte :buffer buffer))
