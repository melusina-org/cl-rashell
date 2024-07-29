;;;; util.lisp — Utilities

;;;; Rashell (https://github.com/melusina-org/cl-rashell)
;;;; This file is part of Rashell.
;;;;
;;;; Copyright © 2017–2023 Michaël Le Barbier
;;;; All rights reserved.

;;;; This file must be used under the terms of the MIT License.
;;;; This source file is licensed as described in the file LICENSE, which
;;;; you should have received as part of this distribution. The terms
;;;; are also available at https://opensource.org/licenses/MIT

(in-package #:rashell/testsuite)

(define-testcase test-string-match ()
  (assert-nil (rashell::string-match "" "supercalifragilisticexpialidocious"))
  (assert-nil (rashell::string-match "?" ""))
  (assert-t (rashell::string-match "" ""))
  (assert-t (rashell::string-match "*" ""))
  (assert-t (rashell::string-match "?" "."))
  (assert-t (rashell::string-match "*" "supercalifragilisticexpialidocious"))
  (assert-t (rashell::string-match "*.c" ".c"))
  (assert-t (rashell::string-match "*.*" "compiler.c"))
  (assert-t (rashell::string-match "super*cali*" "supercalifragilisticexpialidocious"))
  (assert-t (rashell::string-match
	     "supercal?fragilisticexpialidocious"
	     "supercalifragilisticexpialidocious")))

(define-testcase util-testsuite()
  "Run tests for the util module."
  (test-string-match))

;;;; End of file `util.lisp'
