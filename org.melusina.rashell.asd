;;;; org.melusina.rashell.asd — Resilient replicant Shell Programming Library for Common Lisp

;;;; Rashell (https://github.com/melusina-org/cl-rashell)
;;;; This file is part of Rashell.
;;;;
;;;; Copyright © 2017–2023 Michaël Le Barbier
;;;; All rights reserved.

;;;; This file must be used under the terms of the MIT License.
;;;; This source file is licensed as described in the file LICENSE, which
;;;; you should have received as part of this distribution. The terms
;;;; are also available at https://opensource.org/licenses/MIT

(asdf:defsystem #:org.melusina.rashell
  :description "Resilient replicant Shell Programming Library for Common Lisp"
  :author "Michaël Le Barbier"
  :license "MIT"
  :depends-on ("alexandria" "cl-ppcre" "parse-float" "sb-posix")
  :components
  ((:module "src"
    :components ((:file "package")
		 (:file "util")
                 (:file "rashell")
                 (:file "posix")
		 (:file "mktemp")))))

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
		 (:file "mktemp")
                 (:file "testsuite")))))

(asdf:defsystem #:org.melusina.rashell/development
  :description "Development tools for Rashell"
  :author "Michaël Le Barbier"
  :license "MIT License"
  :depends-on (#:org.melusina.atelier)
  :components
  ((:module "libexec/lisp"
    :components ((:file "development")))))

;;;; End of file `org.melusina.rashell.asd'
