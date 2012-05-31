(in-package :ruby-parser)

(deftype proper-list (&optional element)
  (declare (ignore element))
  'list)

(defmacro defvariant (name-and-options &body clauses)
  (let ((variant-name (first (ensure-list name-and-options))))
    `(progn
       (defstruct ,name-and-options)
       ,@(iter (for (name . slot-specs) in clauses)
               (collect
                   `(defstruct (,name (:include ,variant-name))
                      ,@(loop for slot-spec in slot-specs collect
                              (destructuring-bind (slot-name slot-type &key (init-form '(required-argument))) slot-spec
                                `(,slot-name ,init-form :type ,slot-type)))))))))
