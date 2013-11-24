; Vector Memory

(require 'cl)
(require 'z80-memory)
(provide 'z80-memory-vector)

(defstruct (z80-memory-vector (:include z80-memory))  vec)

(defun z80-memory-vector-byte (memory address)
  "Private function which returns the byte in the MEMORY at the given ADDRESS"
  (let* ((v (z80-memory-vector-vec memory))
         (l (length v))
         (a (mod address l)))
    (aref v a)))

(defun z80-memory-vector-from-vector (vector)
  "Make a Z80-MEMORY out of a VECTOR of characters"
  (make-z80-memory-vector :read-byte #'z80-memory-vector-byte :vec vector))
