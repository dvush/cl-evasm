(in-package #:cl-evasm)

(defparameter *jump-dest-size-bytes* 2)
(defparameter +max-uint+ (1- (expt 2 256)))

(defun integer-to-push (int)
  (assert (<= 0 int +max-uint+) (int))
  (let ((bytes-needed (max (ceiling (integer-length int) 8)
			   1)))
    (make-instance 'push-evm :n bytes-needed :data int)))

(defun string-to-push (string)
  (let ((string (if (and (>= (length string) 2)
			 (string= "0x" (subseq string 0 2)))
		    (subseq string 2)
		    string)))
    (integer-to-push (bitsmash:hex->int string))))

(defun asm-raw (ins)
  (cond ((typep ins 'integer) (list (integer-to-push ins)))
	((stringp ins) (list (string-to-push ins)))
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
  (let ((label-positions (make-hash-table)))
    (let ((offset 0))
      (dolist (ins asm)
	(when (typep ins 'label-evm)
	  (assert (null (gethash (get-sym ins) label-positions)) ()
		  "Jump label ~a occurs multiple times: ~a" (get-sym ins))
	  (setf (gethash (get-sym ins) label-positions) offset))
	(let ((ins-length (cond ((typep ins 'push-label-evm) (1+ *jump-dest-size-bytes*))
				((typep ins 'label-evm) 1)
				(t (length (to-bytecode ins))))))
	  (setf offset (+ offset ins-length)))))
    (loop for ins in asm
	  collect (cond ((typep ins 'push-label-evm)
			 (let ((label-pos (gethash (get-sym ins) label-positions)))
			   (unless label-pos
			     (error "Jumplabel can't be resolved ~a" (get-sym ins)))
			   (first (asm (push *jump-dest-size-bytes* label-pos)))))
			((typep ins 'label-evm)
			 (first (asm jumpdest)))
			(t ins)))))

(defun emit-bytecode (asm)
  (let ((asm (resolve-jumplabels asm)))
    (bitsmash:hex<-
     (apply #'concatenate '(vector (unsigned-byte 8))
	    (loop for i in asm
		  collect (let ((bytecode (to-bytecode i)))
			    (unless bytecode
			      (error "Failed to emit bytecode for instruction ~a" i))
			    bytecode))))))

