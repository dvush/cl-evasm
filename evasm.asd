;;;; cl-evasm.asd

(asdf:defsystem #:evasm
  :description "Assembly for EVM"
  :author "Vitaly Drogan <vitaly@dvush.net>"
  :license  "MIT"
  :version "0.0.1"
  :serial t
  :depends-on (#:alexandria #:trivia #:bit-smasher)
  :components ((:module "src"
		:components ((:file "package")
			     (:file "assembly")
			     (:module "examples"
				      :components ((:file "spray")))))))
