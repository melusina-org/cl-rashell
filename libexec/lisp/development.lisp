;;;; development.lisp — Project Development for Rashell

;;;; Rashell (https://github.com/melusina-org/cl-rashell)
;;;; This file is part of Rashell.
;;;;
;;;; Copyright © 2017–2023 Michaël Le Barbier
;;;; All rights reserved.

;;;; This file must be used under the terms of the MIT License.
;;;; This source file is licensed as described in the file LICENSE, which
;;;; you should have received as part of this distribution. The terms
;;;; are also available at https://opensource.org/licenses/MIT

(defpackage #:org.melusina.rashell/development
  (:use #:cl)
  (:local-nicknames
   (#:atelier #:org.melusina.atelier))
  (:export
   #:lint
   #+quicklisp
   #:reload))

(in-package #:org.melusina.rashell/development)

(defun system-relative-pathname (pathname)
  (flet ((system-source-directory ()
	   (asdf:system-source-directory #.(string-downcase (package-name *package*)))))
    (merge-pathnames pathname (system-source-directory))))

(defun system-relative-pathnames (&rest pathnames)
  (mapcar #'system-relative-pathname pathnames))

(defparameter *parameter-bindings*
  '((:copyright-holder . "Michaël Le Barbier")
    (:copyright-year . "2017–2023")
    (:project-name . "Rashell")
    (:project-filename . "org.melusina.rashell")
    (:project-description . "Resilient replicant Shell Programming Library for Common Lisp")
    (:homepage . "https://github.com/melusina-org/cl-rashell")
    (:license . :mit)))

(defun lint ()
  (let ((atelier:*parameter-bindings* *parameter-bindings*))
    (atelier:lint
     (system-relative-pathnames
      #p"org.melusina.rashell.asd"
      #p"doc"
      #p"example"
      #p"src"
      #p"subr"
      #p"testsuite"
      #p"libexec/lisp/development.lisp"))))

#+quicklisp
(defun reload ()
  (ql:quickload '("org.melusina.atelier"
		  "org.melusina.rashell"
		  "org.melusina.rashell/testsuite"
		  "org.melusina.rashell/development")))


;;;;
;;;; Command Stock
;;;;

#+nil
(org.melusina.rashell/development:reload)

;;;; End of file `development.lisp'
