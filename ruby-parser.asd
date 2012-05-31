(asdf:defsystem :ruby-parser
  :description "Ruby Parser"
  :author "Tomohiro Matsuyama"
  :depends-on (:alexandria
               :iterate
               :anaphora
               :optima
               :yacc
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
