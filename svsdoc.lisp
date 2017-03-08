;;; svsdoc.lisp
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

;;; Note: I've attempted to make this portable, but the function #'arglist is nonstandard and only available in CCL.

(in-package :svsdoc)

(defparameter *entity-table*
  '((#\< . "&lt;")
    (#\> . "&gt;")
    (#\& . "&amp;")
    (#\" . "&quot;")))

(defun fn-arglist (fname)
  "Returns arglist for function named fname"
  #+CCL (ccl:arglist fname t)
  #+SBCL (sb-introspect:function-lambda-list fname))

(defun fn-name (f)
  (check-type f function)
  #+CCL (ccl:function-name f)
  #+SBCL (sb-impl::%fun-name f))

(defun htmlify (string stream)
  "Replace forbidden characters in string with HTML entities."
  (loop for char across string do
    (let ((entity (cdr (assoc char *entity-table*))))
      (if entity
          (write-string entity stream)
          (if (> (char-code char) 126)
              (let ((*print-base* 10))
                (write-string "&#" stream) 
                (format stream "~D" (char-code char))
                (write-char #\; stream))
              (write-char char stream))))))

(defun htmlify-format (stream format-arg colon? at? &rest rest)
  (declare (ignore colon? at? rest))
  (htmlify (format nil "~:A" format-arg) stream))

(defgeneric thing-to-html (thing stream)
  (:documentation "Print thing as to stream HTML."))

(defmethod thing-to-html :around (thing stream)
  "Ensure that thing-to-html does not return anything."
  (declare (ignore thing stream))
  (call-next-method)
  (values))

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

(defmethod thing-to-html ((fn function) stream)
  "Shows the description of a function or macro."
  (%thing-to-html fn stream))

(defmethod thing-to-html ((class standard-class) stream)
  "Shows the description of a class."
  (let* ((*print-case* :downcase)
         (class-instance-slots
          (remove :instance (class-slots class)
                  :test (complement #'eq)
                  :key #'slot-definition-allocation)))
    (format stream "~%<TABLE CELLPADDING=3 WIDTH=\"100%\">")
    (print-topline class stream)
    ;(print-documentation-section class stream)
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
    (format stream "~%<TD ALIGN=LEFT><B><code><font size=+1>~/svsdoc::htmlify-format/ </font></code></B>" sym)
    (format stream "~%<TD ALIGN=RIGHT><I>~A</I></TD></TR>" thingname)
    (when (constantp sym)
      (format stream "~%<TR><TD><i>Value: </i>~S</TD></TR>" (symbol-value sym)))))

(defmethod print-topline ((sm standard-method) stream)
  "Makes the top line for a method."
  (let ((*print-case* :downcase)
        (qualifiers (method-qualifiers sm))
        (specializers (method-specializers sm)))
    (format stream "~%<TR>")
    (format stream "~%<TD ALIGN=LEFT><B><CODE>~/svsdoc::htmlify-format/ </B>" (generic-function-name (method-generic-function sm)))
    (format stream "~{~S ~}~/svsdoc::htmlify-format/</code></TD>" qualifiers (form-specialized-arglist specializers (fn-arglist sm)))
    (format stream "~%<TD ALIGN=RIGHT><I>~A</I></TD></TR>" (if (typep sm 'standard-accessor-method)
                                                        "[accessor-method]"
                                                        "[method]"
                                                        ))))

(defmethod print-topline ((gf generic-function) stream)
  "Makes the top line for a generic function."
  (let ((*print-case* :downcase))
    (format stream "~%<TR>")
    (format stream "~%<TD ALIGN=LEFT><B><code><font size=+1>~/svsdoc::htmlify-format/ </font></B>" (generic-function-name gf))
    (format stream "~/svsdoc::htmlify-format/</code></TD>" (fn-arglist gf))
    (format stream "~%<TD ALIGN=RIGHT><I>~A</I></TD></TR>" "[Generic function]")))

(defmethod print-topline ((fn function) stream)
  "Makes the top line for a function or macro."
  (let ((*print-case* :downcase)
        (name (fn-name fn)))
    (format stream "~%<TR>")
    (format stream "~%<TD ALIGN=LEFT><B><code><font size=+1>~/svsdoc::htmlify-format/ </font></B>" name)
    (format stream "~/svsdoc::htmlify-format/</code></TD>" (fn-arglist name))
    (format stream "~%<TD ALIGN=RIGHT><I>~/svsdoc::htmlify-format/</I></TD></TR>" (if (macro-function name)
                                                             "[Macro]"
                                                             "[Function]"))))

(defmethod print-topline ((class standard-class) stream)
  "Makes the top line for a class."
  (let ((*print-case* :downcase)
        (name (class-name class)))
    (format stream "~%<TR>")
    (format stream "~%<TD ALIGN=LEFT><B><code><font size=+1>~/svsdoc::htmlify-format/ </font></B>" name)
    (format stream "~/svsdoc::htmlify-format/</code></TD>" (mapcar 'class-name (class-direct-superclasses class)))
    (format stream "~%<TD ALIGN=RIGHT><I>~/svsdoc::htmlify-format/</I></TD></TR>" "[Class]")))

(defgeneric print-documentation-section (thing stream)
  (:documentation "Prints the documentation section for thing."))

(defun print-docs (docs stream)
  (let ((doc (or docs "[No documentation found]")))
    (format stream "~%<TR>")
    (when doc (format stream "~%<TD COLSPAN=2>~/svsdoc::htmlify-format/</TD>" doc))
    (format stream "~%</TR>")))

(defmethod print-documentation-section ((thing t) stream)
  "Makes default documentation section."
  (print-docs (documentation thing t) stream))

(defmethod print-documentation-section ((sym symbol) stream)
  "Makes documentation section for a variable."
  (print-docs (documentation sym 'variable) stream))

(defmethod print-documentation-section ((fn function) stream)
  "Makes documentation section for a function or macro."
  (print-docs (documentation (fn-name fn) 'function) stream))

(defmethod print-documentation-section ((slot slot-definition) stream)
  (format stream "~%<TR>")
  (format stream "~%<TD>~/svsdoc::htmlify-format/</TD><TD ALIGN=LEFT>~/svsdoc::htmlify-format/</TD>" (slot-definition-name slot) (or (documentation slot t) ""))
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

(defmacro document-package (&rest rest)
  `(%document-package ,@rest :genesis-message ,(format nil "(document-package ~{~S ~})" rest)))

(defun print-package-docs (package stream &key (external t) (internal nil) (functions nil) (macros nil) (generic-functions nil) (classes nil) (variables nil))
  "Do a mass conversion of documentation from a package into HTML."
  (let ((external-symbols nil)
        (internal-symbols nil)
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
    (setq internal-symbols (loop for x being each present-symbol of package
                                 collect x))
    
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
               (setq found-variables (cons symbol found-variables)))))
      
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
      (format stream "<h2>Documentation for package :~A</h2>" (package-name package))
      (format stream "<i>This documentation was created by svsdoc</i>")
      (format stream "<p>")
      (when variables
        (dolist (f found-variables)
          (thing-to-html f stream)
          (format stream "<BR/>~%")
          ))
      (when macros
        (dolist (f found-macros)
          (thing-to-html (macro-function f) stream)
          (format stream "<BR/>~%")
          ))
      (when classes
        (dolist (c found-classes)
          (thing-to-html (find-class c) stream)
          (format stream "<BR/>~%")
          ))
      (when functions
        (dolist (f found-functions)
          (thing-to-html (symbol-function f) stream)
          (format stream "<BR/>~%")
          ))
      (when generic-functions
        (dolist (gf found-gfs)
          (thing-to-html (symbol-function gf) stream)
          (format stream "<BR/>~%")
          )))))
           
#|
(with-open-file (s "ccl:svsdoc.html" :direction :output :if-exists :supersede)
  (svsdoc:print-package-docs :svsdoc s :external t :internal t :variables t :functions t :macros t :classes t :generic-functions t))
|#