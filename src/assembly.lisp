(in-package #:cl-evasm)

(defparameter *jump-dest-size-bytes* 2)
(defparameter +max-uint+ (1- (expt 2 256)))

(defun integer-to-push (int)
  (assert (<= 0 int +max-uint+) (int))
  (let ((bytes-needed (max (ceiling (integer-length int) 8)
			   1)))
    (make-instance 'push-evm :n bytes-needed :data int)))

(defun asm-raw (ins)
  (cond ((typep ins 'integer) (list (integer-to-push ins)))
	((and (symbolp ins)
	      (gethash ins *atom-mnemonics*))
	 (list (funcall (gethash ins *atom-mnemonics*))))
	((and (listp ins)
	      (gethash (car ins) *list-mnemonics*))
	 (list (apply (gethash (car ins) *list-mnemonics*) (cdr ins))))
	((typep ins 'instruction) (list ins))
	((listp ins)
	 (loop for i in ins
	       append (asm-raw i)))
	(t (error "Incorrect assembly instruction ~a" ins))))

(defmacro asm (&rest ins)
  `(asm-raw
    (list
     ,@(loop for i in ins
	     collect
	     (cond ((gethash i *atom-mnemonics*) `(quote ,i))
		   ((and (listp i) (gethash (car i) *list-mnemonics*))
		    `(list (quote ,(car i)) ,@(cdr i)))
		   (t `(asm-raw (list ,i))))))))

(defun resolve-jumplabels (asm)
  (error "incorrect impl")
  (let ((label-positions (make-hash-table)))
    (loop for ins in asm
	  sum (cond ((typep ins 'push-label-evm) +jump-dest-size-bytes+)
		    ((typep ins 'label-evm) 1)
		    (t (length (to-bytecode ins))))
	  into current-offset

	  do (when (typep ins 'label-evm)
	       (when (gethash (get-sym ins) label-positions)
		 (error "Jump label occurs multiple times, label: ~a" (get-sym ins)))
	       (setf (gethash (get-sym ins) label-positions) current-offset)))
    (loop for ins in asm
	  collect (cond ((typep ins 'push-label-evm)
			 (let ((label-pos (gethash (get-sym ins) label-positions)))
			   (unless label-pos
			     (error "Jumplabel can't be resolved ~a" (get-sym ins)))
			   (first (asm (push +jump-dest-size-bytes+ label-pos)))))
			((typep ins 'label-evm)
			 (first (asm jumpdest)))
			(t ins)))))

(defun emit-bytecode (asm)
  (apply #'concatenate '(vector (unsigned-byte 8))
	 (loop for i in asm
	       collect (let ((bytecode (to-bytecode i)))
			 (unless bytecode
			   (error "Failed to emit bytecode for instruction ~a" i))
			 bytecode))))

(defun emit-hex (asm)
  (bitsmash:hex<- (emit-bytecode asm)))

