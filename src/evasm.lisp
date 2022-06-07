(in-package #:evasm)

;; quoted examble
'(emit-bytecode
  (asm (push-label 'haha) jump 2 3
      add sub iszero
      (label 'haha) 9))
