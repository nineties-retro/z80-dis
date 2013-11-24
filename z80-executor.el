;
;
;

(require 'cl)
(require 'z80-evaluator)


(defstruct (z80-executor (:include z80-evaluator))
  djnz-t     djnz-nt
  jr-nz-t    jr-nz-nt
  jr-z-t     jr-z-nt
  jr-nc-t    jr-nc-nt
  jp-nz-t    jp-nz-nt
  ret-nz-t   ret-nz-nt
  jp-t       jp-nt
  ret-z-t    ret-z-nt
  jp-z-t     jp-z-nt
  call-z-t   call-z-nt
  ret-nc-t   ret-nc-nt
  jp-nc-t    jp-nc-nt
  call-nc-t  call-nc-nt
  ret-c-t    ret-c-nt
  jp-c-t     jp-c-nt
  call-c-t   call-c-nt
  ret-po-t   ret-po-nt
  jp-po-t    jp-po-nt
  call-po-t  call-po-nt
  ret-pe-t   ret-pe-nt
  jp-pe-t    jp-pe-nt
  call-pe-t  call-pe-nt
  ret-p-t    ret-p-nt
  jp-p-t     jp-p-nt
  call-p-t   call-p-nt
  registers)


(defun z80-register-dec (reg)
  (- reg 1))

(defun z80-register-inc (reg)
  (mod (+ reg 1) 65535))

(defun z80-executor-inc-reg (reg-num)
  (lexical-let ((reg-num reg-num))
    (lambda (executor start-address end-address)
      (let* ((regs (z80-executor-registers executor))
             (v (aref regs reg-num)))
        (aset regs reg-num (z80-register-inc v))
        (funcall (z80-executor-next-evaluator executor) start-address next-address
        next-address))))

(defun z80-executor-dec-reg (reg-num)
  (lexical-let ((reg-num reg-num))
    (lambda (executor start-address end-address)
      (let* ((regs (z80-executor-registers executor))
             (v (aref regs reg-num)))
        (aset regs reg-num (z80-register-dec v))
        next-address))))


(defun z80-exec (m e)
  (make-z80-executor :memory m :next-evaluator e
   :illegal (lambda (evaluator start-address next-address) 
              (throw 'z80-exec 'illegal))
   :nop   (lambda (evaluator start-address next-address) next-address)
   :dec-b (z80-exec-dec-reg 0)
   :inc-b (z80-exec-dec-reg 0)
   :dec-c (z80-exec-dec-reg 1)
   :inc-c (z80-exec-dec-reg 1)
   :dec-d (z80-exec-dec-reg 2)
   :inc-d (z80-exec-dec-reg 2)
   :dec-e (z80-exec-dec-reg 3)
   :inc-e (z80-exec-dec-reg 3)
   :dec-h (z80-exec-dec-reg 4)
   :inc-h (z80-exec-dec-reg 4)
   :dec-l (z80-exec-dec-reg 5)
   :inc-l (z80-exec-dec-reg 5)
   :dec-a (z80-exec-dec-reg 7)
   :inc-a (z80-exec-dec-reg 7)))
