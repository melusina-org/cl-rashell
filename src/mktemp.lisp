;;;; mktemp.lisp — Temporary files and directories

;;;; Rashell (https://github.com/melusina-org/cl-rashell)
;;;; This file is part of Rashell.
;;;;
;;;; Copyright © 2017–2023 Michaël Le Barbier
;;;; All rights reserved.

;;;; This file must be used under the terms of the MIT License.
;;;; This source file is licensed as described in the file LICENSE, which
;;;; you should have received as part of this distribution. The terms
;;;; are also available at https://opensource.org/licenses/MIT

(in-package #:rashell)

(defparameter *mktemp-designator* nil
  "If set, this parameter is a prefix used when generating temporary file templates.

This can be used to distinguish temporary filenames with a prefix mentioning
the application that generated them or an issue number.")

(defparameter *mktemp-keep* nil
  "If set, temporary files and directories are not deleted.")

(defun mktemp-template ()
  "A template for temporary files in our application."
  (let ((filename
	  (if (stringp *mktemp-designator*)
	      (concatenate 'string *mktemp-designator* ".XXXXXXXX")
	      "XXXXXXXX"))
	(tmpdir (or (uiop:getenv "TMPDIR") "/tmp")))
    (concatenate 'string tmpdir "/" filename)))

(define-utility make-temporary-file () nil
  (:program #p"/usr/bin/mktemp"
   :documentation "Run mktemp(1)."
   :rest (list (mktemp-template)))
  :trim t)

(define-utility make-temporary-directory () nil
  (:program #p"/usr/bin/mktemp"
   :documentation "Run mktemp(1)."
   :rest (list "-d" (mktemp-template)))
  :trim t)

(defmacro with-temporary-file ((filespec) &body body)
  "Run BODY commands in a context where FILESPEC is bound to the path of a temporary file."
  (let ((filename (gensym "RASHELL")))
    `(let* ((,filename
	      (make-temporary-file))
	    (,filespec
	      (pathname (copy-seq ,filename))))
       (unwind-protect
	    (progn ,@body)
	 (unless *mktemp-keep*
	   (rm ,filename :force t))))))

(defmacro with-temporary-directory ((filespec) &body body)
  "Run BODY commands in a context where FILESPEC is bound to the pathname of a temporary directory."
  (let ((filename (gensym "RASHELL")))
    `(let* ((,filename
	      (make-temporary-directory))
	    (,filespec
	      (pathname (concatenate 'string ,filename "/"))))
       (unwind-protect
	    (progn ,@body)
	 (unless *mktemp-keep*
	   (rm ,filename :force t :recursive t))))))

;;;; End of file `mktemp.lisp'
