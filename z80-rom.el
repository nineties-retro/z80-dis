;
; Z80-ROM
;

(require 'cl)
(require 'z80-memory)
(require 'z80-ram)
(provide 'z80-rom)

(defstruct (z80-rom (:include z80-memory))  string)


(defun z80-rom-read-byte* (memory address)
  (let* ((v (z80-rom-string memory))
         (l (length v))
         (a (mod address l)))
    (aref v a)))


(defun z80-rom-from-string (string)
  "Make a Z80-MEMORY out of a STRING"
  (make-z80-rom :read-byte #'z80-rom-read-byte*
                :string string
                :close (lambda (ignore) nil)))


(defun z80-rom-from-buffer (buffer)
  "Make a Z80-ROM out of a BUFFER."
  (save-excursion
    (set-buffer buffer)
    (z80-ram-from-string (buffer-string))))
