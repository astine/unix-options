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

(asdf:defsystem #:unix-options
  :version "0.3.1"
  :serial t
  :description "Easy to use command line option parser"
  :author "Andrew Stine stine.drew@gmail.com"
  :license "LLGPL"
  :components ((:file "unix-options")))
