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

(defpackage #:unix-options-tests
  (:use #:cl #:unix-options #:fiveam))

(in-package #:unix-options-tests)

(def-suite unix-options-tests :description "The primary tests for unix options.")

(in-suite unix-options-tests)

(test alpha-numeric?-test 
  (is (unix-options::alpha-numeric? #\a))
  (is (unix-options::alpha-numeric? #\G))
  (is (unix-options::alpha-numeric? #\4))
  (is (not (unix-options::alpha-numeric? #\space)))
  (is (not (unix-options::alpha-numeric? #\return))))

(test map-parsed-options-test
  (let ((a nil) (b nil) (c nil) (d nil) (files nil))
    (labels ((opt-val (option value)
	       (cond ((equal option "a") (setf a value))
		     ((equal option "b") (setf b value))
		     ((equal option "c") (setf c value))
		     ((equal option "d") (setf d value))))
	     (free-val (free-val) 
	       (push free-val files)))
      (map-parsed-options '("-a") '("a" "b") '() #'opt-val #'free-val)
      (is (identity a))
      (is (not b))
      (map-parsed-options '("-afilename") '() '("a" "b") #'opt-val #'free-val)
      (is (equal a "filename"))
      (is (not b))
      (map-parsed-options '("-ab" "--cd" "--d") '("a" "b" "c" "d") '() #'opt-val #'free-val)
      (is (identity a))
      (is (identity b))
      (is (not c))
      (is (identity d))
      (map-parsed-options '("-ab" "--chello" "--d" "hello") '("a" "b") '("c" "d") #'opt-val #'free-val)
      (is (identity a))
      (is (identity b))
      (is (not c))
      (is (equal d "hello"))
      (map-parsed-options '("--a=filename" "--b=" "filename2") '() '("a" "b") #'opt-val #'free-val)
      (is (equal a "filename"))
      (is (equal b ""))
      (setf files nil)
      (map-parsed-options '("ab" "hello" "--" "hello") '() '() #'opt-val #'free-val)
      (is (equal files '("hello" "hello" "ab"))))))

(test getopt-test
  (is (equal (getopt '("-afgo.txt" "--alpha" "stay.txt" "--file" "return.txt" "loop.txt")
		     "af:j" '("alpha" "file="))
	     '("a" "f" "go.txt" "alpha" "file" "return.txt" "--" "stay.txt" "loop.txt")))
  (is (equal (getopt '("-fago.txt" "--alpha" "--" "--file" "return.txt")
		     "af:j" '("alpha" "file="))
	     '("f" "ago.txt" "alpha" "--" "--file" "return.txt")))
  (is (equal (getopt '("-aj" "afgo.txt" "stay.txt" "--" "return.txt" "loop.txt") 
		     "af:j" '("alpha" "file="))
	     '("a" "j" "--" "afgo.txt" "stay.txt" "return.txt" "loop.txt")))
  (is (equal (getopt '("-aj" "afgo.txt" "stay.txt" "--" "return.txt") 
		     "afj:" '("alpha" "file="))
	     '("a" "j" "afgo.txt" "--" "stay.txt" "return.txt")))
  (let ((cli-options '("-ab" "--file" "file.txt" "file2.txt")))
    (is (equal (getopt cli-options "abcf" '("file="))
	       '("a" "b" "file" "file.txt" "--" "file2.txt")))))

(test print-usage-summary-test
  (is (equal
       (with-output-to-string (*standard-output*)
	 (print-usage-summary "sample~%~@{~A~%~}sample"
			      '(("a" "alpha" nil "sample")
				("bd" ("beta" "delta") nil "sample")
				(nil "gamma" t "sample")
				(nil "epsilon" "FILE" "sample")
				("v" nil nil "sample"))))
"sample
  -a, --alpha              sample
  -b, -d, --beta, --delta  sample
      --gamma=PARAMETER    sample
      --epsilon=FILE       sample
  -v                       sample
sample")))

(test with-cli-options-test
  (let ((cli-options '("-asf" "hello" "--input" "file.txt" "--" "more" "less"))
        (orig-cli-options #'cli-options))
    (setf (fdefinition 'cli-options)
          (lambda () '("-asf" "hello" "--input" "file.txt" "--" "more" "less")))
    (with-cli-options ('("-asf" "hello" "--input" "file.txt" "--" "more" "less"))
	(alpha beta sierra &parameters file input)
      (is (identity alpha))
      (is (not beta))
      (is (identity sierra))
      (is (equal file "hello"))
      (is (equal input "file.txt"))
      (is (equal free '("more" "less"))))
    (with-cli-options (cli-options)
	(alpha beta sierra &parameters file input)
      (is (identity alpha))
      (is (not beta))
      (is (identity sierra))
      (is (equal file "hello"))
      (is (equal input "file.txt"))
      (is (equal free '("more" "less"))))
    (with-cli-options ()
	(alpha beta sierra &parameters file input)
      (is (identity alpha))
      (is (not beta))
      (is (identity sierra))
      (is (equal file "hello"))
      (is (equal input "file.txt"))
      (is (equal free '("more" "less"))))
    (is (equal
	 (with-output-to-string (*standard-output*)
	   (with-cli-options ('("-x") t) ()))
"WARNING: Invalid option: x

Usage: /usr/bin/sbcl [OPTIONS]... -- FREE...

  -h, --help  Prints this summary

"))
    (is (equal
	 (with-output-to-string (*standard-output*)
	   (with-cli-options ('("-h") t) ()))
"Usage: /usr/bin/sbcl [OPTIONS]... -- FREE...

  -h, --help  Prints this summary

"))
    (setf (fdefinition 'cli-options) orig-cli-options)))
