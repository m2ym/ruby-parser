(in-package :ruby-parser)

(defun make-env ())

(defun env-find (env name))

(defun env-find-in-current (env name))

(defun env-use (env name))

(defun env-add (env name kind))

(defun env-extend (env &optional dynamic))

(defun env-unextend (env))

 
