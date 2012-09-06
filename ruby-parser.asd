(asdf:defsystem #:ruby-parser
  :description "Ruby Parser for Common Lisp"
  :author "Tomohiro Matsuyama <tomo@cx4a.org>"
  :license "LLGPL"
  :depends-on (#:alexandria
               #:iterate
               #:anaphora
               #:optima
               #:lyacc
               #:cl-ppcre
               #:closer-mop
               #:parse-number)
  :components ((:module "src"
                :serial t
                :components ((:file "packages")
                             (:file "ast")
                             (:file "util")
                             (:file "specials")
                             (:file "source")
                             (:file "env")
                             (:file "lexer")
                             (:file "parser")))))
