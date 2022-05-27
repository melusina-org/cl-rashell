;;;; org.melusina.rashell.asd — Resilient replicant Shell Programming Library for Common Lisp

;;;; Rashell (https://github.com/melusina-org/cl-rashell)
;;;; This file is part of Rashell.
;;;;
;;;; Copyright © 2017–2022 Michaël Le Barbier
;;;; All rights reserved.

;;;; This file must be used under the terms of the MIT License.
;;;; This source file is licensed as described in the file LICENSE, which
;;;; you should have received as part of this distribution. The terms
;;;; are also available at https://opensource.org/licenses/MIT

(asdf:defsystem #:org.melusina.rashell
  :description "Resilient replicant Shell Programming Library for Common Lisp"
  :author "Michaël Le Barbier"
  :license "MIT"
  :depends-on ("alexandria" "cl-ppcre" "parse-float")
  :components
  ((:module "src"
    :components ((:file "package")
		 (:file "util")
                 (:file "rashell")
                 (:file "posix")))))

(asdf:defsystem #:org.melusina.rashell/testsuite
  :description "Resilient replicant Shell Programming Library for Common Lisp"
  :author "Michaël Le Barbier"
  :license "MIT"
  :depends-on ("org.melusina.confidence" "org.melusina.rashell")
  :components
  ((:module "testsuite"
    :components ((:file "package")
		 (:file "util")
                 (:file "rashell")
                 (:file "posix")
                 (:file "testsuite")))))

;;;; End of file `org.melusina.rashell.asd'
