(in-package :ruby-parser)

(defun rb-compile-error (control-string &rest args)
  (apply #'error control-string args))

(defun rb-warning (control-string &rest args)
  (apply #'warn control-string args))
