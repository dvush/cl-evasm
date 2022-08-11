(in-package #:evasm-examples)

(defun do-n-times (body)
  "n should be on top of the stack
body must have mutating stack effect 0 - 0
body can see i while iterating on top of the stack (i.e. 0, 1, .., n - 1)"
  (alexandria:with-gensyms (loop-start loop-finish)
    (asm
     0					; i=0
     (label loop-start)
     (dup 1) (dup 3)			; st: n i   
     eq (push-label loop-finish) jumpi
					; body - should have stack effect 0 - 0
     body
     1 add				; ++i
     (push-label loop-start) jump
     (label loop-finish)
     pop                                ; pop i
     )))


(defun trivial-deploy-code (bytes)
  "Similar to solidity constructor without any arguments.	   
Simply creates a contract with the BYTES as a bytecode."
  (asm
   (push 2 (length bytes)) (push 1 #x0c) (push 1 0) codecopy
   (push-label :ret) jump
   (verbatim bytes)
   (label :ret)
   (length bytes) 0 return))


;; Implements this interface (function name does not matter)
;;
;; sends 1 wei to each address
;;
;; interface ISpray {
;;    function Spray(address[] calldata targets) payable external;
;;}
(defparameter *spray-code*
  (asm
   (+ 4 32) calldataload ; load address array length
   (do-n-times
       (asm (dup 1) ; get loop i
	    32 mul (+ 4 32 32) add ; address offset
	    calldataload

	    ;; send 1 wei
	    0 0 0 0 1 (dup 6) 0 call
	    pop pop))))

(defparameter *spray-deploy*
  (asm-hex (trivial-deploy-code (asm-oct *spray-code*))))
