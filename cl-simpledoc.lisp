;;; cl-simpledoc.lisp
;;; 07-Mar-2017 SVS

;; Copyright (c) 2017, Shannon Spires
;; All rights reserved.

;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions are
;; met:

;;   * Redistributions of source code must retain the above copyright
;;     notice, this list of conditions and the following disclaimer.

;;   * Redistributions in binary form must reproduce the above copyright
;;     notice, this list of conditions and the following disclaimer in
;;     the documentation and/or other materials provided with the
;;     distribution.

;;   * Neither Shannon Spires nor the names of its contributors of the
;;     software may be used to endorse or promote products derived from
;;     this software without specific prior written permission.

;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;; "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;; LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
;; A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
;; HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
;; SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
;; LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
;; DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
;; THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
;; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
;; OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

;;; Extracts Common Lisp documentation strings from symbols in a package and generates html output
;;;   similar to CLtL2 format.

;;; Note: I've attempted to make this portable, but it's only been tested in CCL and Lispworks.

;;; Limitations: Doesn't attempt to deduce what symbols in package have a (setf <symbol>)
;;;  function defined.

(in-package :cl-simpledoc)

(defvar *could-not-document* nil "List of things that could not be documented by print-package-docs")

(defparameter *html-header*
  "<!DOCTYPE html>
  <html>
  <body>
  <div style=\"margin: auto; width:95%;\">"
  "Default HTML header matter")

(defparameter *html-footer*
  "</div>
  </body>
  </html>"
  "Default HTML footer matter for document-package")

(defparameter *entity-table*
  '((#\< . "&lt;")
    (#\> . "&gt;")
    (#\& . "&amp;")
    (#\" . "&quot;")))

(defun fn-arglist (fname)
  "Returns arglist for function named fname"
  #+CCL (ccl:arglist fname t)
  #+SBCL (sb-introspect:function-lambda-list fname)
  #+LISPWORKS
     (typecase fname
       (symbol (lispworks::function-lambda-list fname))
       (method (lispworks::function-lambda-list (method-generic-function fname)))
       (generic-function (lispworks::function-lambda-list fname))))

#| Discussion about the impossibility of writing #'fn-name transportably.
Notice that ANSI defines no function that is the inverse of #'symbol-function.
That is, given the result Y, where Y=(symbol-function X), there's no portable
function one can apply to Y to produce the symbol X again.

It may be that there are some good reasons for that. In general, it's a very
hard problem for a compiler writer, given a function-like object, to determine
what symbol(s) might be bound to that object in their function cell. Some compilers
like CCL seem to incorporate said information in the function object itself (or at
least they have _some_ mechanism for doing it in general, since the function #'ccl::function-name
seems to work on all function-like objects). But others like Lispworks can't do it
very well at all. Lispworks can do it for ordinary functions using #'system::function-name,
but that produces bogus results for generic functions, methods, and accessor functions created
automatically by #'defstruct. (There are probably more exceptions but those are the ones I had
found when I finally quit looking.)

[Above doesn't apply to generic-functions and methods; the MOP defines #'generic-function-name and
#'method-generic-function and closer-mop ensures those work for all major implementations.]

Another reason why this might be hard is that (setf <fn>) forms complicate things.
Figuring out when a function <fn> has an equivalent setf form is AFAIK very hard, and 
many lisps don't handle (fdefinition '(setf <fn>)) properly in all cases even when
the (setf <fn>) is required by ANSI.

For the above reasons we're abandoning any attempt to make a portable #'fn-name function.
Fortunately, we don't need it. Since the code herein starts with symbols that name things,
all we need to do is keep those symbols around.
|#

#+IGNORE
(defun fn-name (f)
  "Returns name of function designator f"
  #+CCL (ccl:function-name f)
  #+SBCL (sb-impl::%fun-name f)
  #+LISPWORKS
  (typecase f
    (symbol f)
    (method (generic-function-name (method-generic-function f)))
    (generic-function (generic-function-name f))
    (function (system::function-name f))))

(defclass function-designator ()
  ((fd-name :initarg :fd-name :initform nil :accessor fd-name
            :documentation "The symbol that names this function")
   (contents :initarg :contents :initform nil :accessor contents
                    :documentation "The result of calling (symbol-function fd-name) or (macro-function fd-name)"))
  (:documentation "A designator for a function object that keeps its name handy."))

(defun htmlify (string stream)
  "Replace forbidden characters in string with HTML entities."
  (loop for char across string do
    (let ((entity (cdr (assoc char *entity-table*))))
      (if entity
          (write-string entity stream)
          (if (> (char-code char) 126)
              (let ((*print-base* 10))
                (format stream "&#~D;" (char-code char)))
              (write-char char stream))))))

(defun htmlify-format (stream format-arg colon? at? &rest rest)
  (declare (ignore colon? at? rest))
  (htmlify (format nil "~:A" format-arg) stream))

(defgeneric thing-to-html (thing stream)
  (:documentation "Print thing to stream as HTML."))

(defmethod thing-to-html :around (thing stream)
  "Ensure that thing-to-html does not return anything."
  (declare (ignore thing stream))
  (call-next-method)
  (values))

(defmethod thing-to-html ((thing t) stream)
  (declare (ignore stream))
  (push thing *could-not-document*))

(defmethod thing-to-html ((sym symbol) stream)
  (%thing-to-html sym stream))

(defmethod thing-to-html ((gf generic-function) stream)
  "Shows the description of a generic function."
  (let* ((*print-case* :downcase)
         (methods (generic-function-methods gf)))
    (format stream "~%<TABLE CELLPADDING=3 WIDTH=\"100%\">")
    (print-topline gf stream)
    (print-documentation-section gf stream)
    (format stream "<TR><TD COLSPAN=2 ALIGN=RIGHT>")
    (mapc #'(lambda (method)
              (thing-to-html method stream))
          methods)
    (format stream "</TD></TR>~%</TABLE>~%")))

(defmethod thing-to-html ((sm standard-method) stream)
  "Shows the description of a method."
  (%thing-to-html sm stream "95%"))

(defmethod thing-to-html ((fd function-designator) stream)
  "Shows the description of a function or macro."
  (%thing-to-html fd stream))

(defmethod thing-to-html ((class standard-class) stream)
  "Shows the description of a class."
  (let* ((*print-case* :downcase)
         (class-instance-slots
          (remove :instance (class-slots class)
                  :test (complement #'eq)
                  :key #'slot-definition-allocation)))
    (format stream "~%<TABLE CELLPADDING=3 WIDTH=\"100%\">")
    (print-topline class stream)
    (print-documentation-section class stream)
    (format stream "<TR><TD COLSPAN=2 ALIGN=RIGHT>")
    (format stream "~%<TABLE CELLPADDING=3 WIDTH=95%>")
    (dolist (slot class-instance-slots)
      (print-documentation-section slot stream))
    (format stream "</TABLE></TD></TR>~%</TABLE>~%")))

(defun %thing-to-html (thing stream &optional (width "100%"))
  "Shows the description of a function or macro or variable or class."
  (format stream "~%<TABLE CELLPADDING=3 WIDTH=~S>" width)
  (print-topline thing stream)
  (print-documentation-section thing stream)
  (format stream "~%</TABLE>~%"))

(defgeneric print-topline (thing stream)
    (:documentation "Makes the top line of the description of thing in HTML format."))

(defmethod print-topline ((sym symbol) stream)
  "Makes the top line for a variable."
  (let ((*print-case* :downcase)
        (thingname (if (constantp sym) "[Constant]" "[Variable]")))
    (format stream "~%<TR>")
    (format stream "~%<TD ALIGN=LEFT><B><code><font size=+1>~/cl-simpledoc::htmlify-format/ </font></code></B>" sym)
    (format stream "~%<TD ALIGN=RIGHT><I>~A</I></TD></TR>" thingname)
    (when (constantp sym)
      (format stream "~%<TR><TD><i>Value: </i>~S</TD></TR>" (symbol-value sym)))))

(defmethod print-topline ((sm standard-method) stream)
  "Makes the top line for a method."
  (let ((*print-case* :downcase)
        (qualifiers (method-qualifiers sm))
        (specializers (method-specializers sm)))
    (format stream "~%<TR>")
    (format stream "~%<TD ALIGN=LEFT><B><CODE>~/cl-simpledoc::htmlify-format/ </B>" (generic-function-name (method-generic-function sm)))
    (format stream "~{~S ~}~/cl-simpledoc::htmlify-format/</code></TD>" qualifiers (form-specialized-arglist specializers (fn-arglist sm)))
    (format stream "~%<TD ALIGN=RIGHT><I>~A</I></TD></TR>" (if (typep sm 'standard-accessor-method)
                                                        "[accessor-method]"
                                                        "[method]"
                                                        ))))

(defmethod print-topline ((gf generic-function) stream)
  "Makes the top line for a generic function."
  (let ((*print-case* :downcase))
    (format stream "~%<TR>")
    (format stream "~%<TD ALIGN=LEFT><B><code><font size=+1>~/cl-simpledoc::htmlify-format/ </font></B>" (generic-function-name gf))
    (format stream "~/cl-simpledoc::htmlify-format/</code></TD>" (fn-arglist gf))
    (format stream "~%<TD ALIGN=RIGHT><I>~A</I></TD></TR>" "[Generic function]")))

(defmethod print-topline ((fd function-designator) stream)
  "Makes the top line for a function or macro."
  (let ((*print-case* :downcase)
        (name (fd-name fd)))
    (format stream "~%<TR>")
    (format stream "~%<TD ALIGN=LEFT><B><code><font size=+1>~/cl-simpledoc::htmlify-format/ </font></B>" name)
    (format stream "~/cl-simpledoc::htmlify-format/</code></TD>" (fn-arglist name))
    (format stream "~%<TD ALIGN=RIGHT><I>~/cl-simpledoc::htmlify-format/</I></TD></TR>" (if (macro-function name)
                                                             "[Macro]"
                                                             "[Function]"))))

(defmethod print-topline ((class standard-class) stream)
  "Makes the top line for a class."
  (let ((*print-case* :downcase)
        (name (class-name class)))
    (format stream "~%<TR>")
    (format stream "~%<TD ALIGN=LEFT><B><code><font size=+1>~/cl-simpledoc::htmlify-format/ </font></B>" name)
    (format stream "~/cl-simpledoc::htmlify-format/</code></TD>" (mapcar 'class-name (class-direct-superclasses class)))
    (format stream "~%<TD ALIGN=RIGHT><I>~/cl-simpledoc::htmlify-format/</I></TD></TR>" "[Class]")))

(defgeneric print-documentation-section (thing stream)
  (:documentation "Prints the documentation section for thing."))

(defun print-docs (docs stream)
  "Send docs to stream as a table cell."
  (let ((doc (or docs "[No documentation found]")))
    (format stream "~%<TR>")
    (when doc (format stream "~%<TD COLSPAN=2>~/cl-simpledoc::htmlify-format/</TD>" doc))
    (format stream "~%</TR>")))

(defmethod print-documentation-section ((thing t) stream)
  "Makes default documentation section."
  (print-docs (documentation thing t) stream))

(defmethod print-documentation-section ((sym symbol) stream)
  "Makes documentation section for a variable."
  (print-docs (documentation sym 'variable) stream))

(defmethod print-documentation-section ((fd function-designator) stream)
  "Makes documentation section for a function or macro."
  (print-docs (documentation (fd-name fd) 'function) stream))

(defmethod print-documentation-section ((slot slot-definition) stream)
  "Makes documentation section for a slot-definition."
  (format stream "~%<TR>")
  (format stream "~%<TD>~/cl-simpledoc::htmlify-format/</TD><TD ALIGN=LEFT>~/cl-simpledoc::htmlify-format/</TD>" (slot-definition-name slot) (or (documentation slot t) ""))
  (format stream "~%</TR>"))

(defun form-specialized-arglist (specializers arglist)
  "Make and format the specialized lambda list for a method."
  (let ((*print-case* :downcase)
        (separator "(")
        (spa 
         (loop for arg in arglist 
               and spec = (car specializers) do
               (setf specializers (cdr specializers))
               collect
               (if (and spec
                        (not (eq spec (find-class t))))
                 (if (typep spec 'eql-specializer)
                   (list arg (list :eql (eql-specializer-object spec)))
                   (list arg (class-name spec)))
                 arg))))
    (with-output-to-string (stream)
      (dolist (item spa)
        (format stream "~A" separator)
        (setf separator #\space)
        (if (listp item)
          (convert-specializer-pair item stream)
          (format stream "~A" item)
          ))
      (format stream ")"))))

(defun convert-specializer-pair (list stream)
  "Make and format a specific specializer pair in the lambda list of a method."
  (format stream "(~A ~A)" (car list) (cadr list)))

(defun print-package-docs (package stream &key (external t) (internal nil) (functions nil) (macros nil) (generic-functions nil) (classes nil) (variables nil))
  "Do a mass conversion of documentation from a package into HTML.
   Does NOT add proper HTML headers and footers; this way you can document more than one package
   into a single HTML file stream."
  (setf package (find-package package))
  (let ((*could-not-document* nil)
        (external-symbols nil)
        (internal-symbols (when internal (loop for sym being each present-symbol of package
                                           when (eql package (symbol-package sym)) ; omit imported symbols
                                           collect sym)))
        (found-functions nil)
        (found-gfs nil)
        (found-macros nil)
        (function nil)
        (found-classes nil)
        (found-variables nil))
    ; Gotta get both here because class names are always external
    (do-external-symbols (sym package)
      (unless (find-symbol (symbol-name sym) :COMMON-LISP) ; because UIOP reexports these things and COMMON-LISP is adequately documented elsewhere
        (setq external-symbols (cons sym external-symbols))))
    
    ; remove duplicates. Internals should be strictly internals.
    (when internal
      (setf internal-symbols (set-difference internal-symbols external-symbols)))
    
    (flet ((lookup-symbol (symbol)
             (when (and (or functions generic-functions macros)
                        (fboundp symbol))
               (setq function (symbol-function symbol))
               (if (macro-function symbol)
                   (setq found-macros (cons symbol found-macros))
                   (typecase function
                     (generic-function (setq found-gfs (cons symbol found-gfs)))
                     (function (setq found-functions (cons symbol found-functions))))))
             (when (and classes
                        (find-class symbol nil))
               (setq found-classes (cons symbol found-classes)))
             (when (and variables
                        (boundp symbol))
               (setq found-variables (cons symbol found-variables))))
           
           (showdocs (onlist)
             (when variables
               (dolist (f found-variables)
                 (when (member f onlist)
                   (thing-to-html f stream)
                   (format stream "<BR/>~%")
                   )))
             (when macros
               (dolist (f found-macros)
                 (when (member f onlist)
                   (thing-to-html (make-instance 'function-designator
                                    :fd-name f
                                    :contents (macro-function f)) stream)
                   (format stream "<BR/>~%")
                   )))
             (when classes
               (dolist (c found-classes)
                 (when (member c onlist)
                   (thing-to-html (find-class c) stream)
                   (format stream "<BR/>~%")
                   )))
             (when functions
               (dolist (f found-functions)
                 (when (member f onlist)
                   (thing-to-html (make-instance 'function-designator
                                    :fd-name f
                                    :contents (symbol-function f)) stream)
                   (format stream "<BR/>~%")
                   )))
             (when generic-functions
               (dolist (gf found-gfs)
                 (when (member gf onlist)
                   (thing-to-html (symbol-function gf) stream)
                   (format stream "<BR/>~%")
                   )))))
      
      (when external
        (dolist (symbol external-symbols)
          (lookup-symbol symbol)))
      
      (when internal
        (dolist (symbol internal-symbols)
          (lookup-symbol symbol)))
      
      (setf found-functions (sort found-functions #'string-lessp))
      (setf found-gfs (sort found-gfs #'string-lessp))
      (setf found-classes (sort found-classes #'string-lessp))
      (setf found-macros (sort found-macros #'string-lessp))
      (setf found-variables (sort found-variables #'string-lessp))
      ; (format t "~%Functions: ~S" found-functions)
      ; (format t "~%Generic functions: ~S" found-gfs)
      ; (format t "~%Classes: ~S" found-classes)
      (format stream "<h2>Documentation for package :~A</h2>~%" (package-name package))
      (format stream "<i>This documentation was created by <a href=https://github.com/svspire/cl-simpledoc>cl-simpledoc</a></i>~%")
      (format stream "<p>~%")
      
      (when external
        (format stream "<h3 style=\"color:green;\">External Symbols</h3>~%")
        (showdocs external-symbols))
      (when internal
        (format stream "<hr>~%")
        (format stream "<h3 style=\"color:red;\">Internal Symbols</h3>~%")
        (showdocs internal-symbols))
      (when *could-not-document*
        (cons :could-not-document *could-not-document*)))))
           
(defparameter *output-root* (user-homedir-pathname) "Default directory path where document-package will produce its output file.")

(defun document-package (package &optional outpath)
  "Documents given package in its own standalone HTML file at outpath if given,
   otherwise it makes a pathname from the package name and merges it with *output-root*"
  (when (find-package package)
    (let* ((package-name (package-name package))
           (pname (substitute #\- #\/ package-name)) ; slashes are illegal in pathnames
           (outfile (or outpath (merge-pathnames (concatenate 'string pname ".html") *output-root*))))
      (with-open-file (s outfile :direction :output :if-exists :supersede)
        (write-string *html-header* s)
        (print-package-docs package s
                                         :external t
                                         :internal t
                                         :variables t
                                         :functions t
                                         :macros t
                                         :classes t
                                         :generic-functions t)
        (write-string *html-footer* s))
      outfile)))

#|

(document-package :cl-simpledoc)

(with-open-file (s "ccl:cl-simpledoc.html" :direction :output :if-exists :supersede)
  (cl-simpledoc:print-package-docs :cl-simpledoc s :external t :internal t :variables t :functions t :macros t :classes t :generic-functions t))
|#