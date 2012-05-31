(in-package :ruby-parser)

(defstruct (source (:constructor make-source (string)))
  string
  (position 0)
  (lineno 1)
  match-start match-end
  reg-starts reg-ends)

(defun source-read-char (src)
  (with-slots (string position) src
    (prog1 (char string position)
      (incf position))))

(defun source-unread (src substr)
  (with-slots (string position) src
    (setf string (concatenate 'string
                              (subseq string 0 position)
                              substr
                              (subseq string position)))))

(defun source-eos-p (src)
  (= (source-position src) (length (source-string src))))

(defun source-bol-p (src)
  (with-slots (string position) src
    (or (zerop position)
        (char= (char string (1- position)) #\Newline))))

(defun source-scan (regex src &key (advance t))
  (when (stringp regex)
    (setq regex (ppcre:parse-string regex)))
  (multiple-value-bind (match-start match-end reg-starts reg-ends)
      (ppcre:scan `(:sequence :start-anchor ,regex) (source-string src) :start (source-position src))
    (when match-start
      (setf (source-match-start src) match-start
            (source-match-end src) match-end
            (source-reg-starts src) reg-starts
            (source-reg-ends src) reg-ends)
      (when advance
        (setf (source-position src) match-end))
      t)))

(defun source-matched (reg src)
  (if (zerop reg)
      (subseq (source-string src) (source-match-start src) (source-match-end src))
      (subseq (source-string src)
              (elt (source-reg-starts src) (1- reg))
              (elt (source-reg-ends src) (1- reg)))))

(defun source-save-state (src)
  (with-slots #1=(position lineno match-start match-end reg-starts reg-ends) src
    (vector . #1#)))

(defun source-restore-state (src state)
  (with-slots #1=(position lineno match-start match-end reg-starts reg-ends) src
    (loop for i from 0
          for slot-name in '#1#
          do (setf (slot-value src slot-name) (svref state i)))))

(defun make-source-from-stream (stream)
  (make-source
   (with-output-to-string (s)
     (alexandria:copy-stream stream s))))

(defmacro with-source-shorthand (var &body body)
  (once-only (var)
    `(macrolet ((scan (regex) `(source-scan ,regex ,',var))
                (peek (regex) `(source-scan ,regex ,',var :advance nil))
                (matched (&optional (reg 0)) `(source-matched ,reg ,',var)))
       ,@body)))
