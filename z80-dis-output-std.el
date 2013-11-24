;
; Z80-DIS-OUTPUT-STD
;

(require 'cl)
(require 'z80-dis-output)
(require 'z80-memory)
(provide 'z80-dis-output-std)


;
; A concrete output which just outputs values to the standard output.
;
(defstruct (z80-dis-output-std (:include z80-dis-output))
  cached-start-address
  memory)


(defun z80-dis-output-std-pad (nchars)
  (while (not (zerop nchars))
    (write-char ? )
    (setq nchars (- nchars 1))))


(defun z80-dis-output-std-xxx (o start-address end-address nchars)
  (while (< start-address end-address)
    (let* ((memory (z80-dis-output-std-memory o))
	   (get-byte (z80-memory-read-byte memory))
	   (byte (funcall get-byte memory start-address))
	   (next-address (z80-address-next start-address))
	   (hi (lsh byte -4))
	   (lo (logand byte 15)))
      (princ (format "%x" hi))
      (princ (format "%x" lo))
      (setq start-address next-address)
      (setq nchars (- nchars 2))))
  (z80-dis-output-std-pad nchars))



(defun z80-dis-output-std-udisp-to-disp (ud)
  "Convert an unsigned byte displacement UD into a signed displacement."
  (if (> ud 127)
      (- ud 256)
    ud))


(defun z80-dis-output-std-address-delta (pc disp)
  (let ((npc (z80-address-delta pc 2)))
    (z80-address-delta npc (z80-dis-output-std-udisp-to-disp disp))))


(defun z80-dis-output-std-hexify-address (a)
  (format "%04x" a))


(defun z80-dis-output-std (m)
  "Returns a Z80 disassembler output attatched to the standard output."
  (make-z80-dis-output-std
   :memory m
   :start-address 
   (lambda (o a)
     (princ (format "%04x " a))
     (setf (z80-dis-output-std-cached-start-address o) a))
   :end-address 
     (lambda (o a) 
       (let ((s (z80-dis-output-std-cached-start-address o)))
	 (z80-dis-output-std-xxx o s a 9)))
   :start (lambda (o) nil)
   :string (lambda (o s) (princ s))
   :int (lambda (o i) (princ i))
   :address (lambda (o a) (princ (z80-dis-output-std-hexify-address a)))
   :disp 
     (lambda (o disp) 
       (let* ((start (z80-dis-output-std-cached-start-address o))
	      (target (z80-dis-output-std-address-delta start disp)))
	 (princ (z80-dis-output-std-hexify-address target))))
   :end (lambda (o) (terpri))))
