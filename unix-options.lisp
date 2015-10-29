;;; unix-options (C) 2009-2010 Andrew Stine

;;; This software is distributed under the terms of the Lisp Lesser GNU 
;;; Public License (http://opensource.franz.com/preamble.html), also 
;;; known as the LLGPL. 

;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
;;; LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
;;; OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
;;; WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

;;; ----------------------------------------------------------------------

(defpackage #:unix-options
    (:use #:cl)
  (:export #:cli-options
	   #:exe-name
           #:&parameters
           #:&free
	   #:free
	   #:option-spec
	   #:make-option-spec
	   #:short-tokens
	   #:long-tokens
	   #:parameter
	   #:description
	   #:map-parsed-options
	   #:getopt
	   #:with-cli-options
           #:*default-usage-format-string*
	   #:print-usage-summary))

(in-package #:unix-options)

;;definitions:
;;tokens:     space separated items passed in on the command line
;;options:    arguments that can be passed in
;;parameters: arguments that take a value
;;free args:  free tokens not associated with any option

(defvar *default-usage-format-string* "~%~%~@{~A~%~}~%")

(eval-when (:compile-toplevel :load-toplevel)
  (defun ensure-list (val)
    (if (listp val)
        val
	(list val))))

(defmacro aif (test &rest forms)
  `(let ((it ,test))
     (if it ,@forms)))

(defmacro defrestart (name)
  `(defun ,name (c)
     (aif (find-restart ',name)
	  (invoke-restart it))))

(defmacro doseq ((var sequence) &body body)
  (let ((index (gensym))
	(seq (gensym)))
    `(let ((,seq ,sequence))
       (dotimes (,index (length ,seq))
	 (let ((,var (elt ,seq,index)))
	   ,@body)))))

(defmacro with-gensyms (symbols &body body)
  `(let (,@(mapcar (lambda (symbol) 
		     `(,symbol (gensym)))
		   symbols))
	 ,@body))

(defun greater (x y &optional (test #'>))
  (if (funcall test x y)
      x
      y))

(defun greatest (list &optional (test #'>))
  (reduce (lambda (x y)
	    (greater x y test))
	  list))

(defun concat (&rest strings)
  (apply #'concatenate (cons 'string (mapcar #'string strings))))

(defun filter (function list)
  "Filters a list removing all element for which function returns nil"
  (cond ((null list) nil)
	((funcall function (first list))
	 (cons (first list) (filter function (rest list))))
	(t (filter function (rest list)))))

(defun alpha-numeric? (char)
  "Returns true if 'char' is a letter of the English alphabet
   or a numerical digit."
  (let ((code (char-code char)))
    (or (and (> code 47) (< code 58))
	(and (> code 64) (< code 91))
	(and (> code 96) (< code 123)))))

(defun toggle-case (char)
  "Reverses the case of a character"
  (if (upper-case-p char)
      (char-downcase char)
      (char-upcase char)))

(defun split-string (char string)
  (let ((pos (position char string)))
    (cons (subseq string 0 pos)
          (when pos (split-string char (subseq string (1+ pos)))))))

;; -------- option classes --------

(defclass option-spec ()
  ((short-tokens :accessor short-tokens
		 :initarg :short-tokens
		 :initform nil
		 :documentation "A collection of all short tokens valid for this option")
   (long-tokens :accessor long-tokens
		:initarg :long-tokens
		:initform nil
		:documentation "A collection of all long tokens valid for this option")
   (parameter :accessor parameter
	      :initarg :parameter 
	      :initform nil
	      :documentation "A boolean specifying whether this option takes a parameter.
                              Can be a string describing the parameter.")
   (description :accessor description ;TODO: add hooks into documentation
		:initarg :description
		:initform ""
		:documentation "A description of this option's purpose and usage")))

(defmethod make-load-form ((option-spec option-spec) &optional environment)
  ;Defining this method seems necessary to get option-spec to play well with ASDF
  (declare (ignore environment))
  `(make-instance ',(class-of option-spec)
		  :short-tokens ',(short-tokens option-spec)
		  :long-tokens ',(long-tokens option-spec)
		  :parameter ',(parameter option-spec)
		  :description ',(description option-spec)))

(defun tokens-from-symbol (symbol)
  "Returns a lowercase name of symbol and the lowercase first letter of that name"
  (let* ((long-option (string-downcase (string symbol)))
	 (short-option (char long-option 0)))
    (list short-option long-option)))
		  
(defun make-option-spec (tokens &optional parameter description)
  "Creates an option-spec object"
  (let ((option-spec (make-instance 'option-spec)))
    (with-slots (short-tokens long-tokens)
	option-spec
      (flet ((push-symbol (symbol)
	       (destructuring-bind (stoken ltoken)
		   (tokens-from-symbol symbol)
		 (push stoken short-tokens)
		 (push ltoken long-tokens))))
	(setf (parameter option-spec) parameter)
	(setf (description option-spec) (or description "An option"))
	(typecase tokens
	  (list (dolist (token tokens)
		  (typecase token
		    (character (push token short-tokens))
		    (string (push token long-tokens))
		    (symbol (push-symbol token))
		    (t (warn "Bad token: ~A" token))))
		(setf short-tokens (nreverse short-tokens))
		(setf long-tokens (nreverse long-tokens)))
	  (symbol (push-symbol tokens))
	  (t (warn "Bad token specifier: ~A" tokens))))
      option-spec)))

(defun ensure-option-spec (spec)
  (typecase spec 
    (list (apply #'make-option-spec spec))
    (symbol (make-option-spec spec))
    (option-spec spec)
    (t (warn "Invalid Option Spec: ~A" spec))))

(defun tokens (option-spec)
  "Returns all tokens of option-spec"
  (append (mapcar #'string (short-tokens option-spec))
	  (long-tokens option-spec)))

(defun token? (token option-spec)
  "Tests if token is in option spec"
  (member token (tokens option-spec) :test #'equal))

(macrolet ((def-all-lists (name function items)
	     `(defun ,name (,items)
		(mapcan #'copy-list (mapcar ,function ,items)))))

  (def-all-lists all-tokens #'tokens option-specs)
  (def-all-lists all-short-tokens #'short-tokens option-specs)
  (def-all-lists all-long-tokens #'long-tokens option-specs)
  )

(defun option-spec-length (option-spec)
  "Calculates the length of the string necessary to print all of the
   tokens in a standard fashion"
  (+ (greater (+ (* 4 (list-length (short-tokens option-spec))) 2) 6)
     (- (* 4 (list-length (long-tokens option-spec))) 2)
     (apply #'+ (mapcar #'length (long-tokens option-spec)))
     (cond ((null (parameter option-spec))
	    0)
	   ((stringp (parameter option-spec))
	    (1+ (length (parameter option-spec))))
	   (t 10))))
 
(defun option-spec-to-string (option-spec &optional (desc-offset 50))
  "Returns a human readable string describing the option spec."
  (format nil (format nil "~A~~~A,2T~<~@{~A~^ ~:_~}~:>"
		      "~?~:[~;, ~]~?~A"
		      (+ desc-offset 2)
		      (split-string #\Space (description option-spec)))
	  "  ~:[~;~:*~{-~C~^, ~}~]" (list (short-tokens option-spec))
	  (and (short-tokens option-spec) (long-tokens option-spec))
	  "~6,0T~:[~;~:*~{--~A~^, ~}~]" (list (long-tokens option-spec))
	  (cond ((null (parameter option-spec))
		 "")
		((stringp (parameter option-spec))
		 (concat "=" (string-upcase (parameter option-spec))))
		(t "=PARAMETER"))))   

(defun add-token (token option)
  (if (characterp token)
      (pushnew token (short-tokens option))
      (pushnew token (long-tokens option))))

(defun divide-tokens (option-specs)
  "Takes a list of option specifications and returns two lists
   of tokens: one from boolean option specs and one from parameter
   option specs"
  (let ((bool-tokens nil)
	(param-tokens nil))
    (dolist (spec option-specs)
      (if (parameter spec)
	  (setf param-tokens
		(append (tokens spec)
			param-tokens))
	  (setf bool-tokens
		(append (tokens spec)
			bool-tokens))))
    (values bool-tokens param-tokens)))

(defun make-normalized-option-spec (new-spec option-specs)
  "Checks the tokens of new-spec against the tokens in option-specs
   and changes or removes them to avoid conflicts"
  (let ((spec (ensure-option-spec new-spec))
	(used-short-tokens (all-short-tokens option-specs))
	(used-long-tokens (all-long-tokens option-specs)))
    (flet ((not-member (item list &key (test #'eql))
	     (unless (member item list :test test)
	       item)))
      (setf (short-tokens spec)
	    (filter #'identity
		    (mapcar (lambda (token)
			      (or (not-member token used-short-tokens)
				  (not-member (toggle-case token) used-short-tokens)))
			    (short-tokens spec))))
      (setf (long-tokens spec)
	    (filter (lambda (token)
		      (not (member token used-long-tokens :test #'equal)))
		    (long-tokens spec)))
      spec)))

(defmacro add-option-spec (new-spec option-specs)
  `(push (make-normalized-option-spec ,new-spec ,option-specs)
	 ,option-specs))

;; ------------------------------

(defun cli-options ()
  "list of tokens passed in at the cli"
  #+:sbcl (rest sb-ext:*posix-argv*)
  #+:ccl (rest ccl:*command-line-argument-list*)
  #+:clisp (rest ext:*args*)
  #+:lispworks (rest system:*line-arguments-list*)
  #+:cmu (rest extensions:*command-line-words*)
  #+:ecl (rest (ext:command-args))
 )

(defun exe-name ()
  "The command used to execute this program"
  #+:sbcl (first sb-ext:*posix-argv*)
  #+:ccl (first ccl:*command-line-argument-list*)
  #+:clisp (first ext:*args*)
  #+:lispworks (first system:*line-arguments-list*)
  #+:cmu (first extensions:*command-line-words*)
  #+:ecl (first (ext:command-args))
  )

(defun print-usage-summary (description option-specs)
  "Given a description (a control string) and a list of specs for options descriptions
   prints a usage summary. 'Description' is a format control string, which is run
   against a list of strings generated from the specs in 'option-specs' each spec takes 
   the form of a list containing the following values in order:
    short-opts  - a string contain characters used as short options for this option
    long-opt    - either a single string, or a list of strings naming long options
    parameter   - If true, specifies that this option takes a parameter; parameter type 
                  or description can be specified as a string
    description - A short description of this option"
  (let* ((specs (mapcar #'ensure-option-spec option-specs))
	 (max-spec-length (greatest (mapcar #'option-spec-length specs))))
    (format t "~?" description (mapcar (lambda (spec)
					 (option-spec-to-string spec max-spec-length))
				       specs))))

(define-condition bad-option-warning (warning) 
  ((option :initarg :option :reader option)
   (details :initarg :details :reader details))
  (:report (lambda (condition stream)
	     (format stream "~A: ~A"
		     (details condition)
		     (option condition)))))
  
(defun map-parsed-options (cli-options bool-options param-options opt-val-func free-opt-func)
  "A macro that parses a list of command line tokens according to a set of
   conditions and allows the user to operate on them as a series of key/value pairs.
   -- cli-options: a tokenized command line with the executable name removed
   -- bool-options: a list of parameters that do not require values; these are either
           true or false depending on wether they were passed in on the cli.
   -- param-options: a list of parameters that do require values; these are either false,
           if not passed or the value of the next token in the list.
   -- opt-val-func: the code that operates on the key/value pairs; The code in this block is
           executed for every found option name and it associated value (always true
           for boolean parameters) bound to 'option' and 'value' respectively.
   -- free-opt-func: operates on free options not associated with any parameter
  'map-parsed-options' is meant as a backend for convenient option parsing mechanisms such
   as 'with-cli-options' and 'getopts'."
  (macrolet ((dispatch-on-option (option bool-case file-case) ;;a bit of code to determine whether an option is a boolean, file, or invalid.
	       `(let ((_option ,option))
		  (cond ((find _option bool-options :test #'equal)
			 ,bool-case)
			((find _option param-options :test #'equal)
			 ,file-case)
			(t (warn 'bad-option-warning :option _option :details "Invalid option"))))))
    (loop while cli-options do                        ;;loop over the tokens
	 (let ((option (pop cli-options)))
	   (cond ((equal option "--") ;; '--' indicates that all other tokens should be interpreted as free args.
		  (dolist (option cli-options)
		    (funcall free-opt-func option))
		  (return))
                 ((equal option "-") ;; a single hyphen is interpreted as a free arg
                  (funcall free-opt-func option))
		 ((and (equal (char option 0) #\-) ;;if short option(s) '-a' or '-asd'
		       (not (equal (char option 1) #\-)))
		  (doseq (char (subseq option 1)) ;;a set of short options is broken up and looped over separately
		    (dispatch-on-option 
		     (string char)
		     (funcall opt-val-func _option t)
		     (progn
		       (if (= (length option) (1+ (position char option)))
			   (funcall opt-val-func _option (pop cli-options))
			   (funcall opt-val-func _option (subseq option (1+ (position char option)))))
		       (return)))))
		 ((and (equal (char option 0) #\-) ;;if long option
		       (equal (char option 1) #\-)
		       (not (equal (char option 2) #\-)))
		  (aif (position #\= option)
		    (dispatch-on-option
		     (subseq option 2 it)
		     (warn 'bad-option-warning :option _option :details
			   "Used '=' in an option that doesn't take a parameter")
		     (funcall opt-val-func _option (subseq option (1+ it))))
		    (dispatch-on-option 
		     (subseq option 2)
		     (funcall opt-val-func _option t)
		     (funcall opt-val-func _option (pop cli-options)))))
		 (t (funcall free-opt-func option)))))))

(defun getopt (cli-options shortopts longopts)
  "A traditional command-line option parser of a similar general format
as getopt from the Unix cli. Return three values which are all
lists: (1) parsed command-line arguments with \"--\" separating the
valid options and free arguments, (2) only the valid options and (3)
only the free arguments. For example:

  *cli-options* = '(\"-afgo.txt\" \"--alpha\" \"stay.txt\"
                    \"--file\" \"return.txt\" \"loop.txt\")

  (getopts *cli-options* \"af:j\" '(\"alpha\" \"file=\")
   => (\"a\" \"f\" \"go.txt\" \"alpha\" \"file\" \"return.txt\" \"--\"
       \"stay.txt\" \"loop.txt\")
      (\"a\" \"f\" \"go.txt\" \"alpha\" \"file\" \"return.txt\")
      (\"stay.txt\" \"loop.txt\")"
  (let ((bool-options nil)
	(param-options nil)
	(files nil)
	(parsed-options nil))
    (dotimes (index (length shortopts))  ;;parse shortopts into the parameters lists
      (when (alpha-numeric? (char shortopts index))
	(if (and (< index (1- (length shortopts)))
		 (equal (char shortopts (1+ index)) #\:))
	    (push (string (char shortopts index)) param-options)
	    (push (string (char shortopts index)) bool-options))))
    (dolist (opt longopts)               ;;parse longopts into the parameters lists
      (if (equal (char opt (1- (length opt))) #\=)
	  (push (string-right-trim "=" opt) param-options)
	  (push opt bool-options)))
    ;;loop over the option/value pairs pushing them into parsed-options
    (map-parsed-options cli-options bool-options param-options 
			#'(lambda (option value)
			    (push option parsed-options)
			    (unless (or (null value) (equal value t))
			      (push value parsed-options)))
			#'(lambda (option)
			    (push option files)))
    (setf parsed-options (nreverse parsed-options)
          files (nreverse files))
    (values (if files
                (append parsed-options (list "--") files)
                parsed-options)
            parsed-options
            files)))

(defmacro with-cli-options ((&optional (cli-options '(cli-options)) enable-usage-summary) option-variables &body body)
"The macro automatically binds passed in command line options to a set of user defined variable names,
following the usual GNU conventions.

OPTION-VARIABLES is a lambda list, similar to a macro lambda list, of the form:

\({option-variable}* [&parameters {option-variable}*] [&free free-token])

Each OPTION-VARIABLE is either a symbol or a lambda list of the form:

\(symbol &optional option-spec) 

The variable SYMBOL specifies the name of a value to be bound to some value passed in on the cli.
The symbol will be bound to the value of the parsed option, (either as a boolean representing 
whether the option was passed, or as a string, representing the parameter passed, if the option
take parameters,) within the body of WITH-CLI-OPTIONS.

OPTION-SPEC is either an optional option-spec object, which defines how the tokens should be
interpreted to bind this value, or a list that defines one. If option spec is omitted, an
option-spec object is generated internally for this value, using the variable name (SYMBOL) as a
long-form token and the first letter as a short-form token. If the lowercase of the short-form
token is already taken, then the capital version is used. If this is also taken, the option will
have no default short-form token.

Alternatively, one may pass a string as OPTION-SPEC, in which case, the optoin-spec object will
still be generated from SYMBOL, but will use the string as the option description.

Option-variables listed after the &PARAMETERS modifier will be set as options which take a
parameter, unless overridden with an explicitly passed option-spec object.

Lastly, if the &FREE modifier is specified, it should be followed by exactly one symbol, which
will be used as the name of the variable to be bound to the list of free tokens encountered after
all other options."
  (let ((var-bindings nil)
	(var-setters nil)
	(param-options? nil)
	(free-tokens 'free)
	(option-specs nil))
    ;;loop over symbols and lists in option-variables, generating option specifications and
    ;;forms for binding and assigned value to the variables
    (block nil 
      (dolist (option-v option-variables)
	(destructuring-bind (symbol &optional option-spec)
            (ensure-list option-v)
	  (case symbol
	    (&parameters (setf param-options? t))
	    (&free (setf free-tokens (car (last option-variables)))
		   (return))
	    (otherwise
	     (add-option-spec (cond ((or (null option-spec) (stringp option-spec))
				     (make-option-spec symbol param-options? option-spec))
				    (t (ensure-option-spec option-spec)))
			      option-specs)
	     (push `(,symbol nil) var-bindings)
	     (push `((token? option ,(first option-specs))
		     (setf ,symbol value))
		   var-setters))))))
    (flet ((code ()  ;form the main block of code so it can be optionally wrapped with a handler-case
	     (multiple-value-bind (bool-options param-options)
		 (divide-tokens option-specs)
	       `(let ,(cons `(,free-tokens nil) var-bindings)
		  (map-parsed-options ,cli-options
				      ',bool-options ',param-options
				      (lambda (option value)
					(cond ,@var-setters))
				      (lambda (free-val)
					(push free-val ,free-tokens)))
		  (setf ,free-tokens (nreverse ,free-tokens))
		  ,@body))))
      (if enable-usage-summary
	  (progn
	    (push (make-option-spec '(#\h "help") nil "Prints this summary") option-specs)
	    (let ((print-summary-code `(print-usage-summary ,(if (stringp enable-usage-summary) enable-usage-summary
								 (concat "Usage: "
									 (exe-name)
									 " [OPTIONS]... -- "
									 (string-upcase (string free-tokens))
									 "..."
									 *default-usage-format-string*))
							    ',(reverse option-specs))))
	      (push `((token? option ,(first option-specs))
		      (progn ,print-summary-code value
			     (return-from with-cli-options)))
		    var-setters)
	      `(block with-cli-options (handler-case ,(code) (bad-option-warning (c)
							       (format t "WARNING: ~A: ~A~%~%" (details c) (option c))
							       ,print-summary-code)))))
	  (code)))))
