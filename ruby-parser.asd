(asdf:defsystem :ruby-parser
  :description "Ruby Parser for Common Lisp"
  :author "Tomohiro Matsuyama <tomo@cx4a.org>"
  :license "LLGPL"
  :depends-on (:alexandria
               :iterate
               :anaphora
               :alt-match
               :alt-yacc
               :cl-ppcre
               :closer-mop
               :parse-number)
  :components ((:module "src"
                :serial t
                :components ((:file "packages")
                             (:file "util")
                             (:file "specials")
                             (:file "types")
                             (:file "ast")
                             (:file "source")
                             (:file "env")
                             (:file "lexer")
                             (:file "parser")))))
