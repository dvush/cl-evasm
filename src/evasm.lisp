(in-package #:evasm)

(asm-hex
 (push-label 'haha)
 jump 2 3
 add sub iszero
 (label 'haha) 9)

(asm-hex
 16)

(asm-hex
 0 0 (log 0))
