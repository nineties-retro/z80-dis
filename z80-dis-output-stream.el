;
; Z80-DIS-OUTPUT-STREAM
;

(require 'cl)
(require 'z80-dis-output)
(provide 'z80-dis-output-stream)


;
; A concrete output which just outputs values to the standard output.
;
(defstruct (z80-dis-output-stream (:include z80-dis-output))
  stream
  cached-start-address
  memory)


(defun z80-dis-output-stream-pad (stream nchars)
  (while (not (zerop nchars))
    (write-char ?  stream)
    (setq nchars (- nchars 1))))


(defun z80-dis-output-stream-xxx (o start-address end-address nchars)
  (let* ((stream (z80-dis-output-stream-stream o))
         (memory (z80-dis-output-stream-memory o))
         (get-byte (z80-memory-read-byte memory)))
    (while (< start-address end-address)
      (let* ((byte (funcall get-byte memory start-address))
             (next-address (z80-address-next start-address))
             (hi (lsh byte -4))
             (lo (logand byte 15)))
        (princ (format "%x" hi) stream)
        (princ (format "%x" lo) stream)
        (setq start-address next-address)
        (setq nchars (- nchars 2))))
    (z80-dis-output-stream-pad stream nchars)))



(defun z80-dis-output-stream-udisp-to-disp (ud)
  "Convert an unsigned byte displacement UD into a signed displacement."
  (if (> ud 127)
      (- ud 256)
    ud))


(defun z80-dis-output-stream-address-delta (pc disp)
  (let ((npc (z80-address-delta pc 2)))
    (z80-address-delta npc (z80-dis-output-stream-udisp-to-disp disp))))


(defun z80-dis-output-stream-hexify-address (a)
  (format "%04x" a))


(defun z80-dis-output-stream (memory stream)
  "Returns a Z80 disassembler on MEMORY sending output to STREAM."
  (make-z80-dis-output-stream
   :stream stream
   :memory memory
   :start-address 
   (lambda (o a)
     (princ (format "%04x " a) (z80-dis-output-stream-stream o))
     (setf (z80-dis-output-stream-cached-start-address o) a))
   :end-address 
     (lambda (o a) 
       (let ((s (z80-dis-output-stream-cached-start-address o)))
         (z80-dis-output-stream-xxx o s a 9)))
   :start (lambda (o) nil)
   :string (lambda (o s) (princ s (z80-dis-output-stream-stream o)))
   :int (lambda (o i) (princ i (z80-dis-output-stream-stream o)))
   :address (lambda (o a)
              (princ (z80-dis-output-stream-hexify-address a)
                     (z80-dis-output-stream-stream o)))
   :disp 
     (lambda (o disp) 
       (let* ((start (z80-dis-output-stream-cached-start-address o))
              (target (z80-dis-output-stream-address-delta start disp)))
         (princ (z80-dis-output-stream-hexify-address target)
                (z80-dis-output-stream-stream o))))
   :end (lambda (o) (terpri (z80-dis-output-stream-stream o)))))
