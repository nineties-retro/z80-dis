;
; Z80-RAM
;

(require 'cl)
(require 'z80-memory)
(provide 'z80-ram)

(defstruct (z80-ram (:include z80-memory))  write-byte string)

(defun z80-ram-read-byte* (memory address)
  (let* ((v (z80-ram-string memory))
         (l (length v))
         (a (mod address l)))
    (aref v a)))


(defun z80-ram-write-byte* (memory address value)
  (let* ((v (z80-ram-string memory))
         (l (length v))
         (a (mod address l)))
    (aset v a value)))



(defun z80-ram-from-string (string)
  "Make a Z80-RAM out of a STRING."
  (make-z80-ram :read-byte #'z80-ram-read-byte*
                :write-byte #'z80-ram-write-byte*
                :string string
                :close (lambda (ignore) nil)))


(defstruct (z80-ram-buffer (:include z80-ram))  buffer)


(defun z80-ram-buffer-update (ram)
  (let ((buffer (z80-ram-buffer-buffer ram))
        (string (z80-ram-buffer-string ram)))
    (save-excursion
      (set-buffer buffer)
      (goto-char (point-min))
      (insert string))))



(defun z80-ram-from-buffer (buffer)
  "Make a Z80-RAM out of a BUFFER.  The buffer is (eventually) updated with
any changes made to the RAM."
  (save-excursion
    (set-buffer buffer)
    (make-z80-ram-buffer :read-byte #'z80-ram-read-byte*
                         :write-byte #'z80-ram-write-byte*
                         :string string
                         :buffer buffer
                         :close #'z80-ram-buffer-update)))
