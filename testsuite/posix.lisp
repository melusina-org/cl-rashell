;;;; posix.lisp — Posix utilities

;;;; Rashell (https://github.com/melusina-org/cl-rashell)
;;;; This file is part of Rashell.
;;;;
;;;; Copyright © 2017–2022 Michaël Le Barbier
;;;; All rights reserved.

;;;; This file must be used under the terms of the MIT License.
;;;; This source file is licensed as described in the file LICENSE, which
;;;; you should have received as part of this distribution. The terms
;;;; are also available at https://opensource.org/licenses/MIT

(in-package #:org.melusina.rashell/testsuite)


;;;;
;;;; Rashell POSIX Interface
;;;;

(define-testcase test-posix/find-expr ()
  (assert-equal
   '("(" "-not" "(" "-name" "*.c" ")" ")" "-and" "(" "-print" ")")
   (rashell::find-predicate-to-argv '(:and (:not (:name "*.c")) :print))))

(define-testcase posix-testsuite ()
  "Run tests for the posix module."
  (test-posix/find-expr))

;;;; End of file `posix.lisp'
