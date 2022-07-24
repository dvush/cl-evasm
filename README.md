# Intro

This is EVM assembly implemented as a Common Lisp embedded language. 

What it means is that this package provides macro that allows the user to write
evm assembly code with simple syntax while having full general purpose language (Common Lisp)
available to augment asm code writing with arbitrary code. 

See "Advanced, meta evaluation" what is possible with this approach that is impossible with other
assemblies.

The goal of this code is to 
1. Show how one can possibly implement simple and powerful assembly.
(it's only ~200 lines of code, where 90 is just instruction enumeration)
2. Introduce real macros as a useful feature that implementors of evm asm may consider 
3. I really needed asm and I actually use it.

# Rules

Rules for asm writing. 

We use `asm`, `asm-hex`, `asm-oct` macros to transform evm assembly DSL to list of assembly mnemonics,
`mnemonict->octets` function is used to transform the output of `asm` into hex bytecode.

## Literals and mnemonics

### Basic

Numbers and hex strings are translated to push instruction of appropriate size.
```lisp
(asm 16) ;; =>  '((push 1 16))
(asm-hex 16) ;; =>  "6010"
(asm "0xaabb") ;; => '((PUSH 2 43707))))'
```


For each evm opcode there is a mnemonic defined in `src/assembly.lisp`.
- Most mnemonics are just symbols from evm spec.: `add` `pop` 
- Some mnemonics are lists of repetitive symbols from spec. `(push 1 #xff)` `(dup 11)` `(swap 5)` `(log 1)`
- There are special mnemonics 
  1. `(invalid #xaabbccff)` - sequence of bytes that represent a sequence of arbitrary invalid instructions
  2. `(push-label 'symbol')`, `(label 'symbol)` - symbolic labels of some place in bytecode. `label` will resolve
      to `jumpdest` and `push-label` will resolve to `push` of label position to stack.

```lisp
(asm 1 2 add) ;; > '((push 1 1) (push 1 2) add)'
(asm 2 2 add 4 eq
     (push-label 'ok) jumpi
     0 0 revert
     (label 'ok)) ;; adds 2 and 2 and if its not 4 we revert, if its 4 we jump over revert
```

### Arbitrary evaluation

If asm sees a form that is not literal or mnemonic it evaluates is, evaluation should return valid input to asm macro.

```lisp
;; here (+ 2 3) will be evaluated and result is pushed 
(asm (+ 2 3)) ;; -> '((push 1 5))

;; imagine we have function that calcuates selector for function
;; (selector "Transfer()") ;; -> "0xaabbccdd" (not correct btw)
(asm (selector "Transfer()") eq) ;; compares top of the stack to selector of transfer
```

This functionality can be used to replace huff macros as a lisp functions

```lisp
(defun compare-to-n (n)
  (asm n eq))
  
(asm 4 (compare-to-n 4)) ;; when executed will yield 1 at the top of the stack
(asm 5 (compare-to-n 4)) ;; when executed will yield 0 at the top of the stack
```


We can emulate static memory allocation or free storage pointer with this.

```lisp
(let* ((free-memory-slot -32)
       (allocated-memory-slot-1 (incf free-memory-slot 32)) ;; its 0
       (allocated-memory-slot-2 (incf free-memory-slot 32))) ;; its 32
  (asm allocated-memory-slot-1 mload 
       allocated-memory-slot-2 mload add)) ;; adds memory slots at 0 and 32 to each other
```


### Advanced, meta evaluation

This class features are not available in any evm assembly implementation out there, and it's where embedding into lisp shines. 

Imagine we would like to implement a feature like that:
1. We have operation `hide` 
2. Hide takes any valid assembly code and transforms it to code that does not see a value that is on top of the stack.
3. In other words, hide hides top of the stack from the code inside!

Usage:
```lisp
(asm 1 2 3
     (hide add)) ;; will push 1 2 3 to the top of the stack 
	                     ;; add inside hide will not see 3 and will add 1 and 2. 
```

I will not implement hide, but this is trivial code transformation left as an exercise to the reader.

Possible implementation - hide will be common lisp macro (or function, but syntax would be `(hide (asm ...))` instead of `(hide ...)`). Hide will take asm instructions and transform them in a way that code inside will never see hidden value (e.g. `(swap n)`, `(dup n)` will be replaced with `(swap (+ n 1))` `(swap (+ n 1))`).


#### Other uses of meta evaluation 

(meta evaluation is basically macros but not like huff macros which are just functions in lisp terms)

1. `if`,`then`,`else` - can be used instead of arbitrary jumps and labels.
2. Loops. Loop can be a macro that takes code inside loop (body), adds loop counter logic around and hides loop counter data from body.

See [Factor language](https://factorcode.org) for inspiration of useful stack utilities, especially [combinators](https://docs.factorcode.org/content/article-combinators.html)
