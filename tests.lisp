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

(test alpha-numeric?-test 
  (is (unix-options::alpha-numeric? #\a))
  (is (unix-options::alpha-numeric? #\G))
  (is (unix-options::alpha-numeric? #\4))
  (is (not (unix-options::alpha-numeric? #\space)))
  (is (not (unix-options::alpha-numeric? #\return))))

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


(run 'alpha-numeric?-test)
(run 'getopt-test)
