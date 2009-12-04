;;; unix-options (C) 2009 Andrew Stine

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
           #:&parameters
           #:&free
	   #:free
	   #:map-parsed-options
	   #:getopt
	   #:with-cli-options))

(in-package #:unix-options)

;;definitions:
;;tokens:     space separated items passed in on the command line
;;options:    arguments that can be passed in
;;parameters: arguments that take a value
;;free args:  free arguments not associated with any parameter

(defmacro expand-list (list)
  (if (listp list)
      `(list ,@list)
       list))

(defmacro aif (test &rest forms)
  `(let ((it ,test))
     (if it ,@forms)))

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

(defun cli-options ()
  "list of tokens passed in at the cli"
  #+:SBCL (rest sb-ext:*posix-argv*)
  #+:CCL (rest *command-line-argument-list*)
  #+:CLISP (rest ext:*args*)
  #+:LISPWORKS (rest system:*line-arguments-list*)
  #+:CMU (rest extensions:*command-line-words*)
 )

(defun exe-name ()
  "The command used to execute this program"
  #+:SBCL (first sb-ext:*posix-argv*)
  #+:CCL (first *command-line-argument-list*)
  #+:CLISP (first ext:*args*)
  #+:LISPWORKS (first system:*line-arguments-list*)
  #+:CMU (first extensions:*command-line-words*)
  )

(defun greatest (list &key (measure #'identity) (predicate #'>))
  (reduce (lambda (x y) (if (funcall predicate 
				     (funcall measure x)
				     (funcall measure y))
			    x
			    y))
	  list))

(defun print-usage-summary (option-specs detailed-description)
  "option-specs take the form of (short-opt, long-opt, parameter?, description)"
  (labels ((to-list (item)
	     (if (atom item)
		 (list item)
		 item))
	   (parameter-string (parameter)
	     (cond ((stringp parameter) (concatenate 'string "=" parameter))
		   ((null parameter) "")
		   (t "=PARAMETER")))
	   (make-raw-option-spec-string (option-spec)
	     (let ((so (to-list (first option-spec)))
		   (lo (to-list (second option-spec))))
	       (list 
		(format nil "~?~:[~;,~]~?~A" 
			"  ~:[~;~:*~{~A~^, ~}~]" (list so)
			(and so lo)
			"~6,1:T~:[~;~:*~{~A~^, ~}~]" (list lo)
			(parameter-string (third option-spec)))
		(fourth option-spec)))))
    (let* ((specs (mapcar #'make-raw-option-spec-string option-specs))
	   (max-spec-length (length (greatest (mapcar #'first specs) :measure #'length))))
      (format t (concatenate 'string "~:{~A~" (write-to-string (1+ max-spec-length)) "t~A~%~}~A~%")
	      specs
	      detailed-description))))




      (dolist (option-spec option-specs)
	(format t (concatenate 'string "  ~{~A, ~}~{~A~^, ~}~A~" 
			       (write-to-string option-length) ",1t~A~%") 
		(to-list (first option-spec)) 
		(to-list (second option-spec)) 
		(parameter-string (third option-spec))
		(fourth option-spec)))
      (if detailed-description
	  (format t "~A~%" detailed-description)))))

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
			(t (warn (format nil "Bad option: ~A~%" option)))))))
    (loop while cli-options do                        ;;loop over the tokens
	 (let ((option (pop cli-options)))
	   (cond ((equal option "--")
		  (dolist (option cli-options)
		    (funcall free-opt-func option))
		  (return))
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
		     (warn (format nil "Used '=' in an option that doesn't take a parameter: ~A~%" _option))
		     (funcall opt-val-func _option (subseq option (1+ it))))
		    (dispatch-on-option 
		     (subseq option 2)
		     (funcall opt-val-func _option t)
		     (funcall opt-val-func _option (pop cli-options)))))
		 (t (funcall free-opt-func option)))))))

(defun alpha-numeric? (char)
  "Returns true if 'char' is a letter of the English alphabet
   or a numerical digit."
  (let ((code (char-code char)))
    (or (and (> code 47) (< code 58))
	(and (> code 64) (< code 91))
	(and (> code 96) (< code 123)))))

(defun getopt (cli-options shortopts longopts)
  "A more traditional command line option parser of a similar
   general format as getopt from the Unix cli, invoked as such:
   *cli-options* = '(\"-afgo.txt\" \"--alpha\" \"stay.txt\" \"--file\" \"return.txt\" \"loop.txt\")
   (getopts *cli-options* \"af:j\" '(\"alpha\" \"file=\")
   =>  '(\"a\" \"f\" \"go.txt\" \"alpha\" \"file\" \"return.txt\" \"--\" \"stay.txt\" \"loop.txt\") "
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
    (if files
	(reverse (nconc files (list "--")  parsed-options))
	(reverse parsed-options))))

(defmacro with-cli-options ((&optional (cli-options '(cli-options))) option-variables &body body)
  "The macro automatically binds passed in command line options to a set of user defined variable names.

   The list 'option-variables' contains a list of names to which 'with-cli-options' can bind the cli
   options. Any (lowercase) longform option is bound the option-variable of the same name, lowercase short
   options are bound to the first listed option variable beginning with that letter, and uppercase short
   options are bound to the second listed option variable beginning with that letter. Variable names imply 
   boolean parameters, unless listed after '&parameters' in which case they are file parameters."
  (let ((bool-options nil)
	(param-options nil)
	(var-bindings nil)
	(var-setters nil)
	(param-options? nil)
	(free-tokens 'free))
    ;;loop over the symbols in option-variables filling the bool and file parameter lists and
    ;;generating the code forms for binding and assigning value to the variables
    (block nil 
      (dolist (symbol option-variables)
        (cond ((eql symbol '&parameters)
	       (setf param-options? t))
              ((eql symbol '&free)
               (setf free-tokens (car (last option-variables)))
               (return))
	      (t (flet ((so-not-used? (so) ;'so' = 'short option'
		         (unless (or (find so bool-options :test #'equal) 
			             (find so param-options :test #'equal))
		           so)))
	          (let ((long-option (string-downcase (symbol-name symbol)))
		        (short-option (aif (so-not-used? (string-downcase (subseq (symbol-name symbol) 0 1)))
				        it
				        (so-not-used? (subseq (symbol-name symbol) 0 1))))) ;if downcase shortopt is used; attempt upcase one
	            (push `(,symbol nil) var-bindings)
	            (push `((or (equal option ,long-option) ,(if short-option `(equal option ,short-option)))
		            (setf ,symbol value))
		          var-setters)
	            (if param-options?
		        (progn (push long-option param-options)
			       (if short-option (push short-option param-options)))
		        (progn (push long-option bool-options)
			       (if short-option (push short-option bool-options))))))))))
    `(let ,(cons `(,free-tokens nil) var-bindings)
       (map-parsed-options ,cli-options
                           ',bool-options ',param-options
			   (lambda (option value)
			     (cond ,@var-setters))
			   (lambda (free-val)
			     (push free-val ,free-tokens)))
       ,@body)))
