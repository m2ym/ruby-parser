(in-package :ruby-parser)

;;; Lex State

(deftype lex-state ()
  '(member
    :expr-beg
    :expr-end
    :expr-arg
    :expr-cmdarg
    :expr-endarg
    :expr-mid
    :expr-fname
    :expr-dot
    :expr-class))

;;; Lex Stack State

(defstruct stack-state
  (list '(nil)))

(defun stack-state-top (state)
  (first (stack-state-list state)))

(defun stack-state-push (state value)
  (push value (stack-state-list state)))

(defun stack-state-pop (state)
  (smatch state
    ((stack-state (list nil)) nil)
    ((stack-state (list list (cons a rest)))
     (setf list rest)
     a)))

(defun stack-state-lexpop (state)
  (smatch state
    ((stack-state (list list (list* a b rest)))
     (setf list (cons (or a b) rest)))))

;;; Lex Keyword Info

(defstruct (keyword-info (:constructor make-keyword-info (id0 id1 state)))
  (id0 () :read-only t)
  (id1 () :read-only t)
  (state () :type lex-state :read-only t))

(defun keyword-info (name &optional (errorp t))
  (declare #.standard-optimize-settings)
  (match name
    ("end"       (make-keyword-info :kEND       :kEND          :expr-end))
    ("else"      (make-keyword-info :kELSE      :kELSE         :expr-beg))
    ("case"      (make-keyword-info :kCASE      :kCASE         :expr-beg))
    ("ensure"    (make-keyword-info :kENSURE    :kENSURE       :expr-beg))
    ("module"    (make-keyword-info :kMODULE    :kMODULE       :expr-beg))
    ("elsif"     (make-keyword-info :kELSIF     :kELSIF        :expr-beg))
    ("def"       (make-keyword-info :kDEF       :kDEF          :expr-fname))
    ("rescue"    (make-keyword-info :kRESCUE    :kRESCUE-MOD   :expr-mid))
    ("not"       (make-keyword-info :kNOT       :kNOT          :expr-beg))
    ("then"      (make-keyword-info :kTHEN      :kTHEN         :expr-beg))
    ("yield"     (make-keyword-info :kYIELD     :kYIELD        :expr-arg))
    ("for"       (make-keyword-info :kFOR       :kFOR          :expr-beg))
    ("self"      (make-keyword-info :kSELF      :kSELF         :expr-end))
    ("false"     (make-keyword-info :kFALSE     :kFALSE        :expr-end))
    ("retry"     (make-keyword-info :kRETRY     :kRETRY        :expr-end))
    ("return"    (make-keyword-info :kRETURN    :kRETURN       :expr-mid))
    ("true"      (make-keyword-info :kTRUE      :kTRUE         :expr-end))
    ("if"        (make-keyword-info :kIF        :kIF-MOD       :expr-beg))
    ("defined?"  (make-keyword-info :kDEFINED   :kDEFINED      :expr-arg))
    ("super"     (make-keyword-info :kSUPER     :kSUPER        :expr-arg))
    ("undef"     (make-keyword-info :kUNDEF     :kUNDEF        :expr-fname))
    ("break"     (make-keyword-info :kBREAK     :kBREAK        :expr-mid))
    ("in"        (make-keyword-info :kIN        :kIN           :expr-beg))
    ("do"        (make-keyword-info :kDO        :kDO           :expr-beg))
    ("nil"       (make-keyword-info :kNIL       :kNIL          :expr-end))
    ("until"     (make-keyword-info :kUNTIL     :kUNTIL-MOD    :expr-beg))
    ("unless"    (make-keyword-info :kUNLESS    :kUNLESS-MOD   :expr-beg))
    ("or"        (make-keyword-info :kOR        :kOR           :expr-beg))
    ("next"      (make-keyword-info :kNEXT      :kNEXT         :expr-mid))
    ("when"      (make-keyword-info :kWHEN      :kWHEN         :expr-beg))
    ("redo"      (make-keyword-info :kREDO      :kREDO         :expr-end))
    ("and"       (make-keyword-info :kAND       :kAND          :expr-beg))
    ("begin"     (make-keyword-info :kBEGIN     :kBEGIN        :expr-beg))
    ("__LINE__"  (make-keyword-info :k__LINE__  :k__LINE__     :expr-end))
    ("class"     (make-keyword-info :kCLASS     :kCLASS        :expr-class))
    ("__FILE__"  (make-keyword-info :k__FILE__  :k__FILE__     :expr-end))
    ("END"       (make-keyword-info :klEND      :klEND         :expr-end))
    ("BEGIN"     (make-keyword-info :klBEGIN    :klBEGIN       :expr-end))
    ("while"     (make-keyword-info :kWHILE     :kWHILE-MOD    :expr-beg))
    ("alias"     (make-keyword-info :kALIAS     :kALIAS        :expr-fname))
    (otherwise (when errorp (error "There is no ruby keyword named ~A" name)))))

;;; Lexer Constants

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar +ESC-RE+ "\\\\([0-7]{1,3}|x[0-9a-fA-F]{1,2}|M-[^\\\\]|(C-|c)[^\\\\]|[^0-7xMCc])")

  (defconstant +STR-FUNC-BORING+ #x00)
  (defconstant +STR-FUNC-ESCAPE+ #x01)
  (defconstant +STR-FUNC-EXPAND+ #x02)
  (defconstant +STR-FUNC-REGEXP+ #x04)
  (defconstant +STR-FUNC-AWORDS+ #x08)
  (defconstant +STR-FUNC-SYMBOL+ #x10)
  (defconstant +STR-FUNC-INDENT+ #x20)

  (defconstant +STR-SQUOTE+ +STR-FUNC-BORING+)
  (defconstant +STR-DQUOTE+ (logior +STR-FUNC-BORING+ +STR-FUNC-EXPAND+))
  (defconstant +STR-XQUOTE+ (logior +STR-FUNC-BORING+ +STR-FUNC-EXPAND+))
  (defconstant +STR-REGEXP+ (logior +STR-FUNC-REGEXP+ +STR-FUNC-ESCAPE+ +STR-FUNC-EXPAND+))
  (defconstant +STR-SSYM+   +STR-FUNC-SYMBOL+)
  (defconstant +STR-DSYM+   (logior +STR-FUNC-SYMBOL+ +STR-FUNC-EXPAND+)))

;;; Lexer

(defparameter *lexer* nil)

(defclass lexer ()
  ((command-start :initform t)
   (cmdarg :type stack-state
           :initform (make-stack-state))
   (cond :type stack-state
         :initform (make-stack-state))
   (tern :type stack-state
         :initform (make-stack-state))
   (nest :initform 0)
   (version :initarg :version
            :initform 19)
   (lex-state :type lex-state
              :initform :expr-beg)
   (lex-strterm :initform nil)
   (env :initarg :env
        :initform (make-env))
   (in-def :type fixnum
           :initform 0)
   (in-single :type fixnum
              :initform 0)
   (src :initarg :src
        :reader lexer-src)
   (string-buffer :initform '())
   (lineno :initform nil)
   (comments :initform '()))
  (:metaclass closer-mop:funcallable-standard-class))

(defun heredoc (lexer)
  (declare #.standard-optimize-settings)
  (block nil
    (with-slots (lex-strterm src string-buffer) lexer
      (with-source-shorthand src
        (with-match (simple-vector _ eos func last-line) lex-strterm
          (let* ((indent (plusp (logand func +STR-FUNC-INDENT+)))
                 (expand (plusp (logand func +STR-FUNC-EXPAND+)))
                 (eos-re (format nil (if indent
                                         "[ \\t]*~A(\\r?\\n|\\z)"
                                         "~A(\\r?\\n|\\z)")
                                 eos)))
            (flet ((fail () (rb-compile-error "failed to parse heredoc")))
              (when (source-eos-p src)
                (fail))
              (when (and (source-bol-p src) (scan eos-re))
                (source-unread src last-line)
                (return (values :tSTRING-END eos)))
              (setf string-buffer '())
              (if expand
                  (progn
                    (cond ((scan "#[$@]")
                           (decf (source-position src))
                           (return (values :tSTRING-DVAR (matched))))
                          ((scan "#[{]")
                           (return (values :tSTRING-DBEG (matched))))
                          ((scan "#")
                           (push "#" string-buffer)))
                    (iter (until (peek eos-re))
                          (for c = (tokadd-string lexer func (string #\Newline) nil))
                          (when (eq c nil)
                            (fail))
                          (if (equal c (string #\Newline))
                              (progn
                                (push (string #\Newline) string-buffer)
                                (scan "\\n"))
                              (return-from heredoc
                                (values :tSTRING-CONTENT
                                        (format nil "~{~A~}" (reverse string-buffer)))))
                          (when (source-eos-p src)
                            (fail))))
                  (iter (until (peek eos-re))
                        (scan ".*(\\n|\\z)")
                        (push (matched) string-buffer)
                        (when (source-eos-p src)
                          (fail))))
              (setf lex-strterm (vector :heredoc eos func last-line))
              (return
                (values :tSTRING-CONTENT
                        (format nil "~{~A~}" (reverse string-buffer)))))))))))

(defun heredoc-identifier (lexer)
  (declare #.standard-optimize-settings)
  (block nil
    (with-slots (lex-strterm src string-buffer) lexer
      (with-source-shorthand src
        (let ((line nil)
              (term nil)
              (func +STR-FUNC-BORING+))
          (setf string-buffer '())

          (cond ((scan "(-?)(['\"`])(.*?)\\2")
                 (setq term (matched 2))
                 (unless (equal (matched 1) "")
                   (setq func (logior func +STR-FUNC-INDENT+)))
                 (setq func (logior func
                                    (match term
                                      ("'"  +STR-SQUOTE+)
                                      ("\"" +STR-DQUOTE+)
                                      (_    +STR-XQUOTE+))))
                 (push (matched 3) string-buffer))

                ((scan "-?(['\"`])(?!\\1*\\Z)")
                 (rb-compile-error "unterminated here document identifier"))

                ((scan "(-?)(\\w+)")
                 (setq term "\"")
                 (setq func (logior func +STR-DQUOTE+))
                 (unless (equal (matched 1) "")
                   (setq func (logior func +STR-FUNC-INDENT+)))
                 (push (matched 2) string-buffer))

                (t (return)))
        
          (when (scan ".*\\n")
            (setq line (matched))
          ; TODO src.extra_lines_added += 1
          )

        (setf lex-strterm (vector :heredoc (format nil "~{~A~}" (reverse string-buffer)) func line))

        (if (equal term "`")
            (return (values :tXSTRING-BEG "`"))
            (return (values :tSTRING-BEG "\""))))))))

(defun parse-number-1 (string &key (token :tINTEGER) (start 0) (radix 10))
  (declare #.standard-optimize-settings)
  (let ((sign 1))
    (case (char string start)
      (#\+ (incf start))
      (#\- (setq sign -1) (incf start)))
    (setq string (delete #\_ string))
    (values token
            (* (if (equal string "")
                   0
                   (parse-number:parse-number string :start start :radix radix))
               sign))))

(defun parse-number (lexer)
  (declare #.standard-optimize-settings)
  (with-slots (src lex-state) lexer
    (with-source-shorthand src
      (setf lex-state :expr-end)
      (cond ((scan "\\+") (parse-number lexer))
            ((scan "\\-")
             (multiple-value-bind (token value)
                 (parse-number lexer)
               (values token (- value))))
            ((scan "0[xbd]\\b")
             (rb-compile-error "Invalid numeric format"))
            ((scan "0x([a-fA-F0-9_]+)")
             (parse-number-1 (matched 1) :radix 16))
            ((scan "0b([01_]+)")
             (parse-number-1 (matched 1) :radix 2))
            ((scan "0d([0-9_]+)")
             (parse-number-1 (matched 1)))
            ((scan "0[Oo]?[0-7_]*[89]")
             (rb-compile-error "Illegal octal digit."))
            ((scan "0[Oo]?([0-7_]+)|0[Oo]")
             (parse-number-1 (matched 1) :radix 8))
            ((scan "[\\d_]+_([eE]|\\.)")
             (rb-compile-error "Trailing '_' in number."))
            ((scan "[\\d_]+\\.[\\d_]+([eE][+-]?[\\d_]+)?\\b|[+-]?[\\d_]+[eE][+-]?[\\d_]+\\b")
             (parse-number-1 (matched) :token :tFLOAT))
            ((scan "0\\b")
             (parse-number-1 (matched)))
            ((scan "[\\d_]+\\b")
             (parse-number-1 (matched)))
            (t (rb-compile-error "Bad number format"))))))

(defun parse-quote (lexer)
  (declare #.standard-optimize-settings)
  (with-slots (src lex-state lex-strterm) lexer
    (with-source-shorthand src
      (let (beg nnd short-hand c)
        (if (scan "[a-zA-Z0-9]{1,2}")
            (progn
              (when (= (length (matched)) 2)
                (rb-compile-error "unknown type of %string"))
              (setq c (matched)
                    beg (string (source-read-char src))))
            (setq c "Q"
                  beg (string (source-read-char src))
                  short-hand t))

        (when (or (source-eos-p src)
                  (eq c nil)
                  (eq beg nil))
          (rb-compile-error "unterminated quoted string meets end of file"))

        (match beg
          ("(" (setq nnd ")"))
          ("[" (setq nnd "]"))
          ("{" (setq nnd "}"))
          ("<" (setq nnd ">"))
          (_   (setq nnd beg
                     beg (string #\Null))))

        (let (token
              string
              (value (format nil "%~A~A" c beg)))
          (match c
            ("Q"
             (setq token :tSTRING-BEG
                   string +STR-DQUOTE+
                   value (if short-hand
                             (format nil "%~A" nnd)
                             (format nil "%~A~A" c beg))))
            ("q"
             (setq token :tSTRING-BEG
                   string +STR-SQUOTE+))
            ("W"
             (scan "\\s*")
             (setq token :tWORDS-BEG
                   string (logior +STR-DQUOTE+ +STR-FUNC-AWORDS+)))
            ("w"
             (scan "\\s*")
             (setq token :tAWORDS-BEG
                   string (logior +STR-SQUOTE+ +STR-FUNC-AWORDS+)))
            ("x"
             (setq token :tXSTRING-BEG
                   string +STR-XQUOTE+))
            ("r"
             (setq token :tREGEXP-BEG
                   string +STR-REGEXP+))
            ("s"
             (setf lex-state :expr-fname)
             (setq token :tSYMBEG
                   string +STR-SSYM+)))

          (unless token
            (rb-compile-error "Bad %string type. Expected [Qqwxr\W], found '~A'." c))

          (setf lex-strterm (vector :strterm string nnd beg))

          (values token value))))))

(defun parse-string (lexer)
  (declare #.standard-optimize-settings)
  (block nil
    (with-slots (nest lex-strterm src string-buffer lineno) lexer
      (with-source-shorthand src
        (with-match (simple-vector _ func term paren) lex-strterm
          (let ((space nil)
                (term-re (ppcre:quote-meta-chars term))
                (awords (and func (plusp (logand func +STR-FUNC-AWORDS+))))
                (regexp (and func (plusp (logand func +STR-FUNC-REGEXP+))))
                (expand (and func (plusp (logand func +STR-FUNC-EXPAND+)))))
            (unless func
              (setf lineno nil)
              (return :tSTRING-END))
            (setq space (and awords (scan "\\s+")))
            (when (and (zerop nest) (scan term-re))
              (cond (awords
                     (setf (svref lex-strterm 1) nil)
                     (return :tSPACE))
                    (regexp
                     (setf lineno nil)
                     (return (values :tREGEXP-END (parse-regx-options lexer))))
                    (t
                     (setf lineno nil)
                     (return (values :tSTRING-END term)))))
            (when space
              (return :tSPACE))
            (setf string-buffer '())
            (when expand
              (cond ((scan "#(?=[$@])")
                     (return :tSTRING-DVAR))
                    ((scan "#[{]")
                     (return :tSTRING-DBEG))
                    ((scan "#")
                     (push "#" string-buffer))))
            (when (eq (tokadd-string lexer func term paren) nil)
              (rb-compile-error "unterminated string meets end of file"))
            (return
              (values :tSTRING-CONTENT
                      (format nil "~{~A~}" (reverse string-buffer))))))))))

(defun read-escape (lexer)
  (declare #.standard-optimize-settings)
  (with-slots (src) lexer
    (with-source-shorthand src
      (cond ((scan "\\") "\\")
            ((scan "n")  (string #\Newline))
            ((scan "t")  (string #\Tab))
            ((scan "r")  (string #\Return))
            ((scan "f")  (string #\Linefeed))
            ((scan "v")  (string #\Vt))
            ((scan "a")  (string #\Bel))
            ((scan "e")  (string #\Esc))
            ((scan "b")  (string #\Backspace))
            ((scan "s")  " ")
            ((scan "[0-7]{1,3}")
             (string (code-char (parse-integer (matched) :radix 8))))
            ((scan "x([0-9a-fA-F]{1,2})")
             (string (code-char (parse-integer (matched 1) :radix 16))))
            ((scan "M-\\\\[\\\\MCc]")
             (scan "M-\\\\")
             (aprog1 (read-escape lexer)
               (setf (char it 0) (code-char (logior (char-code (char it 0)) #x80)))))
            ((scan "M-(.)")
             (aprog1 (matched 1)
               (setf (char it 0) (code-char (logior (char-code (char it 0)) #x80)))))
            ((scan "(C-|c)\\\\[\\\\MCc]")
             (scan "(C-|c)\\\\")
             (aprog1 (read-escape lexer)
               (setf (char it 0) (code-char (logand (char-code (char it 0)) #x9f)))))
            ((scan "C-\\?|c\\?")
             (code-char 127))
            ((scan "(?:C-|c)(.)")
             (aprog1 (matched 1)
               (setf (char it 0) (code-char (logand (char-code (char it 0)) #x9f)))))
            ((or (scan "[McCx0-9]")
                 (source-eos-p src))
             (rb-compile-error "Invalid escape character syntax"))
            (t (source-read-char src))))))

(defun parse-regx-options (lexer)
  (declare #.standard-optimize-settings)
  (with-slots (src) lexer
    (with-source-shorthand src
      (scan "[a-z]*")
      (loop for c across (matched)
            if (find c "ixmonesu")
              collect c into good
            else
              collect c into bad
            finally
               (if bad
                   (rb-compile-error "unknown regexp option~P - ~A"
                                     (length bad) bad)
                   (format nil "~{~A~}" good))))))

(defun tokadd-escape (lexer term)
  (declare #.standard-optimize-settings)
  (with-slots (src string-buffer) lexer
    (with-source-shorthand src
      (cond ((scan "\\\\\\n"))
            ((scan "\\\\([0-7]{1,3}|x[0-9a-fA-F]{1,2})")
             (push (matched) string-buffer))
            ((scan "\\\\([MC]-|c)(?=\\\\)")
             (push (matched) string-buffer)
             (tokadd-escape lexer term))
            ((scan "\\\\([MC]-|c)(.)")
             (push (matched) string-buffer))
            ((scan "\\\\[McCx]")
             (rb-compile-error "Invalid escape character syntax"))
            ((scan "\\\\(.)")
             (push (matched) string-buffer))
            (t
             (rb-compile-error "Invalid escape character syntax"))))))

(defun tokadd-string (lexer func term paren)
  (declare #.standard-optimize-settings)
  (with-slots (nest src string-buffer) lexer
    (with-source-shorthand src
      (let ((awords (plusp (logand func +STR-FUNC-AWORDS+)))
            (escape (plusp (logand func +STR-FUNC-ESCAPE+)))
            (expand (plusp (logand func +STR-FUNC-EXPAND+)))
            (regexp (plusp (logand func +STR-FUNC-REGEXP+)))
            (symbol (plusp (logand func +STR-FUNC-SYMBOL+))))
        (let ((paren-re (and paren (ppcre:quote-meta-chars paren)))
              (term-re (ppcre:quote-meta-chars term))
              c)
          (iter (until (source-eos-p src))
                (for handled = t)
                (setq c nil)
                (cond ((and (zerop nest) (scan term-re))
                       (decf (source-position src))
                       (finish))
                      ((and paren-re (scan paren-re))
                       (incf nest))
                      ((scan term-re)
                       (decf nest))
                      ((and awords (scan "\\s"))
                       (decf (source-position src))
                       (finish))
                      ((and expand (scan "#(?=[\\$\\@\\{])"))
                       (decf (source-position src))
                       (finish))
                      ((and expand (scan "#(?!\\n)")))
                      ((peek "\\")
                       (cond ((and awords (scan "\\\\\\n"))
                              (push (string #\Newline) string-buffer)
                              (next-iteration))
                             ((and awords (scan "\\\\\\s"))
                              (setq c " "))
                             ((and expand (scan "\\\\\\n"))
                              (next-iteration))
                             ((and regexp (peek "\\\\"))
                              (tokadd-escape lexer term)
                              (next-iteration))
                             ((and expand (scan "\\\\"))
                              (setq c (read-escape lexer)))
                             ((scan "\\\\\\n"))
                             ((scan "\\\\\\\\")
                              (when escape
                                (push "\\" string-buffer))
                              (setq c "\\"))
                             ((scan "\\\\")
                              (unless (or (scan term-re)
                                          (null paren)
                                          (scan paren-re))
                                (push "\\" string-buffer)))
                             (t (setq handled nil))))
                      (t (setq handled nil)))
                (unless handled
                  (scan (format nil (if awords
                                        "[^~A~A\\#\\0\\\\\\n\\ ]+|."
                                        "[^~A~A\\#\\0\\\\]+|.")
                                term-re paren-re))
                  (setq c (matched))
                  (when (and symbol (ppcre:scan #\Null c))
                    (rb-compile-error "symbol cannot contain '\\0'")))
                (setq c (or c (matched)))
                (push c string-buffer))
          (setq c (or c (matched)))
          (when (source-eos-p src)
            (setq c nil))
          c)))))

(defun unescape (s)
  (declare #.standard-optimize-settings)
  (match s
    ("a"   (string #\Bel))
    ("b"   (string #\Backspace))
    ("e"   (string #\Escape))
    ("f"   (string #\Linefeed))
    ("n"   (string #\Newline))
    ("r"   (string #\Return))
    ("s"   " ")
    ("t"   (string #\Tab))
    ("v"   (string #\Vt))
    ("\\"  "\\")
    (#.(string #\Newline) (string ""))
    ("C-?" (string (code-char 127)))
    ("c?"  (string (code-char 127)))
    (otherwise
     (cond ((ppcre:scan "^[0-7]{1,3}" s)
            (string (code-char (parse-integer s :radix 8 :junk-allowed t))))
           ((ppcre:scan "^x([0-9a-fA-F]{1,2})" s)
            (string (code-char (parse-integer s :start 1 :radix 16 :junk-allowed t))))
           ((ppcre:scan "^M-(.)" s)
            (string (code-char (logior (char-code (char s 2)) #x80))))
           ((ppcre:scan "^C-(.)" s)
            (string (code-char (logand (char-code (char s 2)) #x9f))))
           ((ppcre:scan "^c(.)" s)
            (string (code-char (logand (char-code (char s 1)) #x9f))))
           ((ppcre:scan "^[McCx0-9]" s)
            (rb-compile-error "Invalid escape character syntax"))
           (t s)))))

(defun lex (&optional (lexer *lexer*))
  (declare #.standard-optimize-settings)
  (with-slots (lex-state lex-strterm
               cmdarg cond tern command-start
               lineno comments version)
      lexer
    (with-source-shorthand (lexer-src lexer)
      (flet ((arg-ambiguous ()
               (rb-warning "Ambiguous first argument. make sure."))
             (expr-beg-push ()
               (stack-state-push cond nil)
               (stack-state-push cmdarg nil)
               (setf lex-state :expr-beg))
             (argument-state-p (state)
               (or (eq state :expr-arg)
                   (eq state :expr-cmdarg)))
             (fix-arg-lex-state ()
               (setf lex-state (if (or (eq lex-state :expr-fname)
                                       (eq lex-state :expr-dot))
                                   :expr-arg
                                   :expr-beg))))
        (when lex-strterm
          (return-from lex (lex-string lexer)))
          
        (iter (with space-seen = nil)
              (with command-state = command-start)
              (with src = (lexer-src lexer))
              (with last-state = lex-state)
              (setf command-start nil)

              (when (scan '(:alternation #\Space #\Tab #\Return #\Page #\Vt))
                (setq space-seen t)
                (next-iteration))

              (when (peek "[^a-zA-Z]")
                (cond
                  ((scan "\\n|#")
                   (setf lineno nil)
                   (when (equal (matched) "#")
                     (decf (source-position src))

                     (loop while (scan "\\s*#.*(\\n+|\\z)")
                           do (push (matched) comments))

                     (when (source-eos-p src)
                       (return)))

                   (scan "\\n+")

                   (when (member lex-state '(:expr-beg :expr-fname
                                             :expr-dot :expr-class))
                     (next-iteration))

                   (setf command-start t)
                   (setf lex-state :expr-beg)
                   (return :tNL))

                  ((scan "[\\]\\)\\}]")
                   (stack-state-lexpop cond)
                   (stack-state-lexpop cmdarg)
                   (setf lex-state :expr-end)
                   (let ((result (ematch (matched)
                                   (")" :tRPAREN)
                                   ("]" :tRBRACK)
                                   ("}" :tRCURLY))))
                     (when (member result '(tRBACK tRCURLY))
                       (stack-state-lexpop tern))
                     (return (values result (matched)))))

                  ((scan "\\.\\.\\.?|,|![=~]?")
                   (setf lex-state :expr-beg)
                   (return
                     (ematch (matched)
                       (".."  :tDOT2)
                       ("..." :tDOT3)
                       (","   :tCOMMA)
                       ("!"   :tBANG)
                       ("!="  :tNEQ)
                       ("!~"  :tNMATCH))))

                  ((peek "\\.")
                   (cond ((scan "\\.\\d")
                          (rb-compile-error "no .<digit> floating literal anymore put 0 before dot"))
                         ((scan "\\.")
                          (setf lex-state :expr-dot)
                          (return (values :tDOT ".")))))

                  ((scan "\\(")
                   (let ((result :tLPAREN2))
                     (cond ((or (eq lex-state :expr-beg)
                                (eq lex-state :expr-mid))
                            (setq result :tLPAREN))
                           ((and space-seen
                                 (eq lex-state :expr-cmdarg))
                            (setq result :tLPAREN-ARG))
                           ((and space-seen
                                 (eq lex-state :expr-arg))
                            (stack-state-push tern nil)
                            (rb-warning "don:t put space before argument parentheses")
                            (setq result :tLPAREN2))
                           ((not space-seen)
                            (stack-state-push tern nil)))

                     (expr-beg-push)

                     (return (values result "("))))

                  ((peek "\\=")
                   (cond ((scan "\\=\\=\\=|\\=\\=|\\=~|\\=>|\\=(?!begin\\b)")
                          (fix-arg-lex-state)
                          (return
                            (values
                             (ematch (matched)
                               ("="   :tEQL)
                               ("=="  :tEQ)
                               ("===" :tEQQ)
                               ("=>"  :tASSOC)
                               ("=~"  :tMATCH)
                               ("->"  :tLAMBDA))
                             (matched))))

                         ((scan "\\=begin(?=\\s)")
                          (push (matched) comments)

                          (unless (scan "(?m).*?\\n=end( |\\t|\\f)*[^\\n]*(\\n|\\z)")
                            (setf comments '())
                            (rb-compile-error "embedded document meets end of file"))

                          (push (matched) comments)
                            
                          (next-iteration))
                         (t (error "you shouldn't be able to get here"))))

                  ((scan #.(format nil "\\\"(~A|#(~A|[^\\{\\#\\@\\$\\\"\\\\])|[^\\\"\\\\\\#])*\\\"" +ESC-RE+ +ESC-RE+))
                   (setf lex-state :expr-end)
                   (let ((string (subseq (matched) 1 (1- (length (matched))))))
                     (return (values :tSTRING
                                     (ppcre:regex-replace-all +ESC-RE+
                                                              string
                                                              (lambda (s &rest args)
                                                                (declare (ignore args))
                                                                (unescape s)))))))

                  ((scan "\\\"")
                   (setf lex-strterm (vector :strterm +STR-DQUOTE+ "\"" (string #\Nul)))
                   (return (values :tSTRING-BEG "\"")))

                  ((scan "\\@\\@?\\w*")
                   (when (ppcre:scan (matched) "\\@\\d")
                     (rb-compile-error (format nil "`~A` is not allowed as a variable name" (matched))))

                   (return (process-token lexer (matched) command-state)))

                  ((scan "\\:\\:")
                   (when (or (member lex-state '(:expr-beg :expr-mid :expr-class))
                             (and (argument-state-p lex-state) space-seen))
                     (setf lex-state :expr-beg)
                     (return (values :tCOLON3 "::")))

                   (setf lex-state :expr-dot)
                   (return (values :tCOLON2 "::")))

                  ((and (not (eq lex-state :expr-end))
                        (not (eq lex-state :expr-endarg))
                        (scan ":([a-zA-Z_]\\w*(?:[?!]|=(?!>))?)"))
                   (setf lex-state :expr-end)
                   (return (values :tSYMBOL (matched 1))))

                  ((scan "\\:")
                   (when (or (eq lex-state :expr-end)
                             (eq lex-state :expr-endarg)
                             (peek "\\s")
                             (stack-state-top tern))
                     (setf lex-state :expr-beg)
                     (return (values :tCOLON ":")))

                   (cond ((scan "\\'")
                          (setf lex-strterm (vector :strterm +STR-SSYM+ (matched) (string #\Nul))))
                         ((scan "\\\"")
                          (setf lex-strterm (vector :strterm +STR-DSYM+ (matched) (string #\Nul)))))

                   (setf lex-state :expr-fname)
                   (return (values :tSYMBEG ":")))

                  ((peek "[0-9]")
                   (return (parse-number lexer)))

                  ((scan "\\[")
                   (let ((result (matched)))
                     (cond ((or (eq lex-state :expr-fname)
                                (eq lex-state :expr-dot))
                            (setf lex-state :expr-arg)
                            (cond ((scan "\\]\\=")
                                   (return (values :tASET "[]=")))
                                  ((scan "\\]")
                                   (return (values :tAREF "[]")))
                                  (t
                                   (rb-compile-error "unexpected '['"))))
                           ((or (eq lex-state :expr-beg)
                                (eq lex-state :expr-mid))
                            (stack-state-push tern nil)
                            (setq result :tLBRACK))
                           ((and (argument-state-p lex-state) space-seen)
                            (stack-state-push tern nil)
                            (setq result :tLBRACK))
                           (t
                            (setq result '|[|)))

                     (expr-beg-push)

                     (return (values result "["))))

                  ((scan "\\'(\\\\.|[^\\'])*\\'")
                   (setf lex-state :expr-end)
                   (return (values :tSTRING (matched))))

                  ((peek "\\|")
                   (cond ((scan "\\|\\|\\=")
                          (setf lex-state :expr-beg)
                          (return (values :tOP-ASGN "||")))
                         ((scan "\\|\\|")
                          (setf lex-state :expr-beg)
                          (return (values :tOROP "||")))
                         ((scan "\\|\\=")
                          (setf lex-state :expr-beg)
                          (return (values :tOP-ASGN "|")))
                         ((scan "\\|")
                          (fix-arg-lex-state)
                          (return (values :tPIPE "|")))))
                    
                  ((scan "\\{")
                   #| FIXME
                   if defined?(@hack_expects_lambda) && @hack_expects_lambda
                   @hack_expects_lambda = false
                   self.lex_state = :expr_beg
                   return :tLAMBEG
                   end
                   |#
                     (let ((result (cond ((or (argument-state-p lex-state)
                                              (eq lex-state :expr-end))
                                          :tLCURLY)
                                         ((eq lex-state :expr-endarg)
                                          :tLBRACE-ARG)
                                         (t
                                          (stack-state-push tern t)
                                          :tLBRACE))))
                       (expr-beg-push)
                       (unless (eq result :tLBRACE)
                         (setf command-start t))

                       (return (values result "{"))))
                    
                    ((scan "->")
                     #|
                     FIXME
                     @hack_expects_lambda = true
                     |#
                     (setf lex-state :expr-arg)
                     (return :tLAMBDA))

                    ((scan "[+-]")
                     (let* ((sign (matched))
                            (utype (if (equal sign "+") :tUPLUS :tUMINUS))
                            (type (if (equal sign "+") :tPLUS :tMINUS)))
                       (when (or (eq lex-state :expr-fname)
                                 (eq lex-state :expr-dot))
                         (setf lex-state :expr-arg)
                         (if (scan "@")
                             (return (values utype (format nil "~A@" sign)))
                             (return (values type sign))))

                       (when (scan "=")
                         (setf lex-state :expr-beg)
                         (return (values :tOP-ASGN sign)))
                       
                       (when (or (eq lex-state :expr-beg)
                                 (eq lex-state :expr-mid)
                                 (and (argument-state-p lex-state)
                                      space-seen
                                      (not (peek "\\s"))))
                         (when (argument-state-p lex-state)
                           (arg-ambiguous))

                         (setf lex-state :expr-beg)
                         (return
                           (values (if (peek "\\d")
                                       (if (eq utype :tUPLUS)
                                           (parse-number lexer)
                                           :tUMINUS-NUM)
                                       utype)
                                   sign)))

                       (setf lex-state :expr-beg)
                       (return (values type sign))))

                    ((peek "\\*")
                     (cond ((scan "\\*\\*=")
                            (setf lex-state :expr-beg)
                            (return (values :tOP-ASGN "**")))
                           ((scan "\\*\\*")
                            (fix-arg-lex-state)
                            (return (values :tPOW "**")))
                           ((scan "\\*=")
                            (setf lex-state :expr-beg)
                            (return (values :tOP-ASGN "*")))
                           ((scan "\\*")
                            (let ((result (cond ((and (argument-state-p lex-state)
                                                      space-seen
                                                      (peek "\\S"))
                                                 (rb-warning "`*' interpreted as argument prefix")
                                                 :tSTAR)
                                                ((or (eq lex-state :expr-beg)
                                                     (eq lex-state :expr-mid))
                                                 :tSTAR)
                                                (t
                                                 :tSTAR2))))
                              (fix-arg-lex-state)
                              (return (values result "*"))))))

                    ((peek "\\<")
                     (cond ((scan "\\<=\\>")
                            (fix-arg-lex-state)
                            (return (values :tCMP "<=>")))
                           ((scan "\\<=")
                            (fix-arg-lex-state)
                            (return (values :tLEQ "<=")))
                           ((scan "\\<\\<=")
                            (fix-arg-lex-state)
                            (setf lex-state :expr-beg)
                            (return (values :tOP-ASGN "<<")))
                           ((scan "\\<\\<")
                            (when (and (not (member lex-state '(:expr-end :expr-dot
                                                                :expr-endarg :expr-class)))
                                       (or (not (argument-state-p lex-state))
                                           space-seen))
                              (awhen (heredoc-identifier lexer)
                                (return it)))

                            (fix-arg-lex-state)
                            (return (values :tLSHFT "<<")))
                           ((scan "\\<")
                            (fix-arg-lex-state)
                            (return (values :tLT "<")))))

                    ((peek "\\>")
                     (cond ((scan "\\>=")
                            (fix-arg-lex-state)
                            (return (values :tGEQ ">=")))
                           ((scan "\\>\\>=")
                            (fix-arg-lex-state)
                            (setf lex-state :expr-beg)
                            (return (values :tOP-ASGN ">>")))
                           ((scan "\\>\\>")
                            (fix-arg-lex-state)
                            (return (values :tRSHFT ">>")))
                           ((scan "\\>")
                            (fix-arg-lex-state)
                            (return (values :tGT ">")))))

                    ((scan "\\`")
                     (case lex-state
                       (:expr-fname
                        (setf lex-state :expr-end)
                        (return (values :tBACK-REF2 "`")))
                       (:expr-dot
                        (setf lex-state (if command-state :expr-cmdarg :expr-arg))
                        (return (values :tBACK-REF2 "`")))
                       (otherwise
                        (setf lex-strterm (vector :strterm +STR-XQUOTE+ "`" (string #\Nul)))
                        (return (values :tXSTRING-BEG "`")))))

                    ((scan "\\?")
                     (when (or (eq lex-state :expr-end)
                               (eq lex-state :expr-endarg))
                       (setf lex-state :expr-beg)
                       (stack-state-push tern t)
                       (return (values :tEH "?")))
                     (when (source-eos-p src)
                       (rb-compile-error "incomplete character syntax"))
                     (cond ((peek '(:alternation :whitespace-char-class #\Vt))
                            (unless (argument-state-p lex-state)
                              (awhen (match (char (matched) 0)
                                       (#\Space   #\s)
                                       (#\Newline #\n)
                                       (#\Tab     #\t)
                                       (#\Vt      #\v)
                                       (#\Return  #\r)
                                       (#\Page    #\f))
                                (rb-warning "invalid character syntax; use ?\\~C" it)))
                            (setf lex-state :expr-beg)
                            (stack-state-push tern t)
                            (return (values :tEH "?")))
                           ((peek "\\w(?=\\w)")
                            (setf lex-state :expr-beg)
                            (stack-state-push tern t)
                            (return (values :tEH "?"))))
                     (let ((c (if (scan "\\")
                                  (read-escape lexer)
                                  (string (source-read-char src)))))
                       (setf lex-state :expr-end)
                       (if (= version 18)
                           (return (values :tINTEGER
                                           (logand (char-int (char c 0)) #xff)))
                           (return (values :tSTRING c)))))

                    ((peek "\\&")
                     (cond ((scan "\\&\\&\\=")
                            (setf lex-state :expr-beg)
                            (return (values :tOP-ASGN "&&")))
                           ((scan "\\&\\&")
                            (setf lex-state :expr-beg)
                            (return (values :tANDOP "&&")))
                           ((scan "\\&\\=")
                            (setf lex-state :expr-beg)
                            (return (values :tOP-ASGN "&")))
                           ((scan "\\&")
                            (let ((result (cond ((and (argument-state-p lex-state)
                                                      space-seen
                                                      (not (peek "\\s")))
                                                 (rb-warning "`&' interpreted as argument prefix")
                                                 :tAMPER)
                                                ((or (eq lex-state :expr-beg)
                                                     (eq lex-state :expr-mid))
                                                 :tAMPER)
                                                (t :tAMPER2))))
                              (fix-arg-lex-state)
                              (return (values result "&"))))))

                    ((scan "\\/")
                     (when (or (eq lex-state :expr-beg)
                               (eq lex-state :expr-mid))
                       (setf lex-strterm (vector :strterm +STR-REGEXP+ "/" (string #\Nul)))
                       (return (values :tREGEXP-BEG "/")))

                     (when (scan "\\=")
                       (setf lex-state :expr-beg)
                       (return (values :tOP-ASGN "/")))

                     (when (and (argument-state-p lex-state)
                                space-seen
                                (not (scan "\\s")))
                       (arg-ambiguous)
                       (setf lex-strterm (vector :strterm +STR-REGEXP+ "/" (string #\Nul)))
                       (return (values :tREGEXP-BEG "/")))

                     (fix-arg-lex-state)
                     (return (values :tDIVIDE "/")))

                    ((scan "\\^=")
                     (setf lex-state :expr-beg)
                     (return (values :tOP-ASGN "^")))

                    ((scan "\\^")
                     (fix-arg-lex-state)
                     (return (values :tCARET "^")))

                    ((scan "\\;")
                     (setf command-start t)
                     (setf lex-state :expr-beg)
                     (return (values :tSEMI ";")))

                    ((scan "\\~")
                     (when (or (eq lex-state :expr-fname)
                               (eq lex-state :expr-dot))
                       (scan "@"))

                     (fix-arg-lex-state)
                     (return (values :tTILDE "~")))

                    ((scan "\\\\")
                     (when (scan "\\n")
                       (setf lineno nil)
                       (setf space-seen t)
                       (next-iteration))
                     (rb-compile-error "bare backslash only allowed before newline"))

                    ((scan "\\%")
                     (when (or (eq lex-state :expr-beg)
                               (eq lex-state :expr-mid))
                       (return (parse-quote lexer)))

                     (when (scan "\\=")
                       (setf lex-state :expr-beg)
                       (return (values :tOP-ASGN "%")))

                     (when (and (argument-state-p lex-state)
                                (not (peek "\\s")))
                       (return (parse-quote lexer)))

                     (fix-arg-lex-state)
                     (return (values :tPERCENT "%")))

                    ((peek "\\$")
                     (cond ((scan "(\\$_)(\\w+)")
                            (setf lex-state :expr-end)
                            (return (process-token lexer (matched) command-state)))
                           ((scan "\\$_")
                            (setf lex-state :expr-end)
                            (return (values :tGVAR (matched))))
                           ((scan "\\$[~*$?!@\\/\\\\;,.=:<>\"]|\\$-\\w?")
                            (setf lex-state :expr-end)
                            (return (values :tGVAR (matched))))
                           ((scan "\\$([\\&\\`\\'\\+])")
                            (setf lex-state :expr-end)
                            (if (eq last-state :expr-fname)
                                (return (values :tGVAR (matched)))
                                (return (values :tBACK-REF
                                                (char (matched) 0)))))
                           ((scan "\\$([1-9]\\d*)")
                            (setf lex-state :expr-end)
                            (if (eq last-state :expr-fname)
                                (return (values :tGVAR (matched)))
                                (return (values :tNTH-REF
                                                (parse-integer (subseq (matched) 1 2))))))
                           ((scan "\\$0")
                            (setf lex-state :expr-end)
                            (return (process-token lexer (matched) command-state)))
                           ((scan "\\$\\W|\\$\\z")
                            (setf lex-state :expr-end)
                            (return (values "$" "$")))
                           ((scan "\\S\\w+")
                            (setf lex-state :expr-end)
                            (return (process-token lexer (matched) command-state)))))

                    ((peek "\\_")
                     (cond ((and (source-bol-p src)
                                 (scan "__END__(\n|\Z)"))
                            (setf lineno nil)
                            (return))
                           ((scan "\\_\\w*")
                            (return (process-token lexer (matched) command-state)))))))
                
              (cond ((or (scan '(:alternation #\Eot #\Sub #\Nul))
                         (source-eos-p src))
                     (return))
                    ((scan "\\W")
                     (rb-compile-error "Invalid char ~A in expression" (matched))))
              
              (scan "\\w+")
              (return (process-token lexer (matched) command-state)))))))

(defun process-token (lexer token command-state)
  (declare #.standard-optimize-settings)
  (block nil
    (with-slots (lex-state command-start cmdarg cond tern env) lexer
      (with-source-shorthand (lexer-src lexer)
        (when (and (ppcre:scan "^\\w" token)
                   (scan "[\\!\\?](?!=)"))
          (setq token (concatenate 'string token (matched))))
        (let ((src (lexer-src lexer))
              (last-state lex-state)
              result)
          (cond ((ppcre:scan "^\\$" token)
                 (setf lex-state :expr-end
                       result :tGVAR))
                ((ppcre:scan "^@@" token)
                 (setf lex-state :expr-end
                       result :tCVAR))
                ((ppcre:scan "^@" token)
                 (setf lex-state :expr-end
                       result :tIVAR))
                (t
                 (if (ppcre:scan "[!?]$" token)
                     (setq result :tFID)
                     (progn
                       (when (and (eq lex-state :expr-fname)
                                  (scan "=(?:(?![~>=])|(?==>))"))
                         (setq result :tIDENTIFIER
                               token (concatenate 'string token (matched))))
                       (unless result
                         (setq result (if (ppcre:scan "^[A-Z]" token)
                                          :tCONSTANT
                                          :tIDENTIFIER)))))
                 (when (and (not (stack-state-top tern))
                            (or (and (eq lex-state :expr-beg)
                                     (not command-state))
                                (eq lex-state :expr-arg)
                                (eq lex-state :expr-cmdarg)))
                     #|
                   FIXME
                   if (lex_state == :expr_beg && !command_state) || lex_state == :expr_arg || lex_state == :expr_cmdarg
                   colon = src.scan(/:/)

                   if colon && src.peek(1) != ":"
                   src.unscan
                   self.lex_state = :expr_beg
                   src.scan(/:/)
                   self.yacc_value = [token, src.lineno]
                   return :tLABEL
                   end

                   src.unscan if colon
                   end
                   |#
                   )
                 (unless (eq lex-state :expr-dot)
                   (optima:when-match (keyword-info id0 id1 state)
                       (keyword-info token nil)
                     (let ((old-state lex-state)
                           (value (vector token (source-lineno src))))
                       (declare (ignore value))
                       (setf lex-state state)
                       (when (eq old-state :expr-fname)
                         (return id0))
                       (when (eq id0 :kDO)
                         (setf command-start t)
                         (cond ((stack-state-top cond)
                                (return :kDO-COND))
                               ((and (stack-state-top cmdarg)
                                     (not (eq old-state :expr-cmdarg)))
                                (return :kDO-BLOCK))
                               ((eq old-state :expr-endarg)
                                (return :kDO-BLOCK))
                          ; FIXME defined?(@hack_expects_lambda) && @hack_expects_lambda
                              (t
                               (return :kDO))))
                       (when (or (eq old-state :expr-beg)
                                 (eq old-state :expr-value))
                         (return id0))
                       (unless (eq id0 id1)
                         (setf lex-state :expr-beg))
                       (return id1))))
                 (if (member lex-state '(:expr-beg :expr-mid
                                         :expr-dot :expr-arg
                                         :expr-cmdarg))
                     (setf lex-state (if command-state :expr-cmdarg :expr-arg))
                     (setf lex-state :expr-end))))
          (when (and (not (eq last-state :expr-dot))
                     (eq (env-find env token) :lvar))
            (setf lex-state :expr-end))
          (return (values result token)))))))

(defun lex-string (lexer)
  (declare #.standard-optimize-settings)
  (with-slots (lex-state lex-strterm lineno) lexer
    (multiple-value-bind (token value)
        (if (and lex-strterm
                 (eq (svref lex-strterm 0) :heredoc))
            (heredoc lexer)
            (parse-string lexer))
      (when (or (eq token :tSTRING-END)
                (eq token :tREGEXP-END))
        (setf lineno nil
              lex-strterm nil
              lex-state :expr-end))
      (values token value))))

(defun make-lexer (src &key (version 18))
  (declare #.standard-optimize-settings)
  (let ((lexer (make-instance 'lexer :src src :version version)))
    (flet ((lex ()
             (multiple-value-bind (token value) (lex lexer)
               ;(format t "LEX ~A ~A~%" token value)
               (values token value))))
      (closer-mop:set-funcallable-instance-function lexer #'lex)
      lexer)))

(defun lex-from-stream (stream)
  (let ((lexer (make-lexer (make-source-from-stream stream))))
    (iter (for (values token value) = (funcall lexer))
          (while token)
          (collect (list token value)))))

(defun lex-from-string (string)
  (with-input-from-string (stream string)
    (lex-from-stream stream)))

(defun lex-from-file (filename)
  (with-open-file (stream filename)
    (lex-from-stream stream)))
