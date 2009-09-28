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
  (:export #:*cli-options*
           #:&file-parameters
           #:&free
	   #:do-parsed-options
	   #:with-cli-options
	   #:getopt))

(in-package #:unix-options)

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

(defvar *cli-options*
  #+:SBCL (rest sb-ext:*posix-argv*)
  #+:CCL (rest *command-line-argument-list*)
  #+:CLISP (rest ext:*args*)
  #+:LISPWORKS (rest system:*line-arguments-list*)
  #+:CMU (rest extensions:*command-line-words*)
  "list of tokens passed in at the cli"
 )

(defmacro do-parsed-options ((cli-options bool-params file-params files) &body body)
  "A macro that parses a list of command line tokens according to a set of
   conditions and allows the user to operate on them as a series of key/value pairs.
   -- cli-options: a tokenized command line with the executable name removed
   -- bool-params: a list of parameters that do not require values; these are either
           true or false depending on wether they were passed in on the cli.
   -- file-params: a list of parameters that do require values; these are either false,
           if not passed or the falue of the next token in the list.
   -- files: a place to put all free tokens, (those not associated with an option or
           listed after a '--' on the cli
   -- body: the code that operates on the key/value pairs; The code in this block is
           executed for every found option name and it associated value (always true
           for boolean parameters) bound to 'option' and 'value' respectively.
  'Do-parsed-options' is meant as a backend for convenient option parsing mechanisms such
   as 'with-cli-options' and 'getopts'."
  (with-gensyms (option options)
    (labels ((dispatch-on-option (option bool-case file-case) ;;a bit of code to determine whether an option is a boolean, file, or invalid.
	       `(let ((option ,option))
		  (cond ((find option (expand-list ,bool-params) :test #'equal)
			 ,bool-case)
			((find option (expand-list ,file-params) :test #'equal)
			 ,file-case)
			(t (warn (format nil "Bad option: ~A~%" option)))))))
      `(let ((,options (expand-list ,cli-options)))
	 (loop while ,options do                        ;;loop over the tokens
	      (let ((,option (pop ,options)))
		(cond ((equal ,option "--")
		       (dolist (,option ,options)
			 (push ,option ,files))
		       (return))
		      ((and (equal (char ,option 0) #\-) ;;if short option(s) '-a' or '-asd'
			    (not (equal (char ,option 1) #\-)))
		       (doseq (char (subseq ,option 1)) ;;a set of short options is broken up and looped over separately
			 ,(dispatch-on-option 
			   `(string char)
			   `(let ((value t)) ,@body)
			   `(progn
			      (if (= (length ,option) (1+ (position char ,option)))
				  (let ((value (pop ,options))) ,@body)
				  (let ((value (subseq ,option (1+ (position char ,option))))) 
				    ,@body))
			      (return)))))
		      ((and (equal (char ,option 0) #\-) ;;if long option
			    (equal (char ,option 1) #\-)
			    (not (equal (char ,option 2) #\-)))
		       ,(dispatch-on-option 
			 `(subseq ,option 2)
			 `(let ((value t)) ,@body)
			 `(let ((value (pop ,options))) ,@body)))
		      (t (push ,option ,files)))))))))

(defmacro with-cli-options ((&optional (cli-options *cli-options*)) option-variables &body body)
  "The macro automatically binds passed in command line options to a set of user defined variable names.

   The list 'option-variables' contains a list of names to which 'with-cli-options' can bind the cli
   options. Any (lowercase) longform option is bound the option-variable of the same name, lowercase short
   options are bound to the first listed option variable beginning with that letter, and uppercase short
   options are bound to the second listed option variable beginning with that letter. Variable names imply 
   boolean parameters, unless listed after '&file-parameters' in which case they are file parameters."
  (let ((bool-params nil)
	(file-params nil)
	(var-bindings nil)
	(var-setters nil)
	(file-vars? nil)
	(files nil))
    ;;loop over the symbols in option-variables filling the bool and file parameter lists and
    ;;generating the code forms for binding and assigning value to the variables
    (block nil 
      (dolist (symbol option-variables)
        (cond ((eql symbol '&file-parameters)
	       (setf file-vars? t))
              ((eql symbol '&free)
               (setf files (car (last option-variables)))
               (return))
	      (t (flet ((so-not-used? (so) ;'so' = 'short option'
		         (unless (or (find so bool-params :test #'equal) 
			             (find so file-params :test #'equal))
		           so)))
	          (let ((long-option (string-downcase (symbol-name symbol)))
		        (short-option (aif (so-not-used? (string-downcase (subseq (symbol-name symbol) 0 1)))
				        it
				        (so-not-used? (subseq (symbol-name symbol) 0 1))))) ;if downcase shortopt is used; attempt upcase one
	            (push `(,symbol nil) var-bindings)
	            (push `((or (equal option ,long-option) ,(if short-option `(equal option ,short-option)))
		            (setf ,symbol value))
		          var-setters)
	            (if file-vars?
		        (progn (push long-option file-params)
			       (if short-option (push short-option file-params)))
		        (progn (push long-option bool-params)
			       (if short-option (push short-option bool-params))))))))))
    `(let ,(cons `(,files nil) var-bindings)
       (do-parsed-options (,cli-options ,bool-params ,file-params ,files)
	 (cond ,@var-setters))
       ,@body)))

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
  (let ((bool-params nil)
	(file-params nil)
	(files nil)
	(parsed-options nil))
    (dotimes (index (length shortopts))  ;;parse shortopts into the parameters lists
      (when (alpha-numeric? (char shortopts index))
	(if (and (< index (1- (length shortopts)))
		 (equal (char shortopts (1+ index)) #\:))
	    (push (string (char shortopts index)) file-params)
	    (push (string (char shortopts index)) bool-params))))
    (dolist (opt longopts)               ;;parse longopts into the parameters lists
      (if (equal (char opt (1- (length opt))) #\=)
	  (push (string-right-trim "=" opt) file-params)
	  (push opt bool-params)))
    ;;loop over the option/value pairs pushing them into parsed-options
    (do-parsed-options (cli-options bool-params file-params files)
      (push option parsed-options)
      (unless (or (null value) (equal value t))
	(push value parsed-options)))
    (if files
	(reverse (append files (list "--")  parsed-options))
	(reverse parsed-options))))
