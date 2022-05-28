;;;; cl-evasm.asd

(asdf:defsystem #:cl-evasm
  :description "Describe cl-evasm here"
  :author "Your Name <your.name@example.com>"
  :license  "Specify license here"
  :version "0.0.1"
  :serial t
  :depends-on (#:alexandria #:trivia #:bit-smasher)
  :components ((:module "src"
		:components ((:file "package")
			     (:file "instructions")
			     (:file "assembly")
			     (:file "cl-evasm")))))
