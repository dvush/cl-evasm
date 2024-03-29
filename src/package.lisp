;;;; package.lisp

(uiop:define-package #:evasm
  (:use #:cl)
  (:export
   ;; main enty points
   ;; asm, asm-hex, asm-raw - macro to write evmasm programs
   #:asm
   #:asm-oct
   #:asm-hex

   #:asm-raw
   #:mnemonics->octets
   #:mnemonics->hex

   ;; assembly mnemonics
   #:stop
   #:add
   #:mul
   #:sub
   #:div
   #:sdiv
   #:mod
   #:smod
   #:addmod
   #:mulmod
   #:exp
   #:signextend
   #:lt
   #:gt
   #:slt
   #:sgt
   #:eq
   #:iszero
   #:and
   #:or
   #:xor
   #:not
   #:byte
   #:shl
   #:shr
   #:sar
   #:sha3
   #:address
   #:balance
   #:origin
   #:caller
   #:callvalue
   #:calldataload
   #:calldatasize
   #:calldatadopy
   #:codesize
   #:codecopy
   #:gasprice
   #:extcodesize
   #:extcodecopy
   #:returndatasize
   #:returndatacopy
   #:extcodehash
   #:blockhash
   #:coinbase
   #:timestamp
   #:number
   #:difficulty
   #:gaslimit
   #:chaind
   #:selfbanace
   #:basefee
   #:pop
   #:mload
   #:mstore
   #:mstore8
   #:sload
   #:sstore
   #:jump
   #:jumpi
   #:pc
   #:msize
   #:gas
   #:jumpdest
   #:push
   #:dup
   #:swap
   #:log
   #:create
   #:call
   #:callcode
   #:return
   #:delegatecall
   #:create2
   #:staticcall
   #:revert
   #:invalid
   #:verbatim
   #:push-label
   #:label
   #:selfdestuct))

(uiop:define-package #:evasm-examples
  (:use #:cl #:evasm))
