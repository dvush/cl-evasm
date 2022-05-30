;;;; cl-evasm.lisp

(in-package #:cl-evasm)


(asm
 0 calldataload) 

(asm 0 1 add)

(defun hevm-exec (code)
  (uiop:run-program (list "hevm" "exec" "--code" code "--gas" "10000000" "--jsontrace") :output '(:string :stripped t)))

'(hevm-exec
 (emit-bytecode
  (asm (push-label 'haha) jump 1 2 3 (label 'haha) 4
       0 mstore 32 0 return))) 


(emit-bytecode
 (asm "0xffee" calldataload 42 eq
      (push-label 'haha) jumpi 1 2 3 (label 'haha) 4
       0 mstore 32 0 return)) 

;;; erc20 token
