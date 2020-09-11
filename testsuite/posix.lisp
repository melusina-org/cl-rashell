;;;; posix.lisp -- Posix utilities

;;;; Rashell (https://github.com/michipili/cl-rashell)
;;;; This file is part of Rashell
;;;;
;;;; Copyright © 2017–2020 Michaël Le Barbier
;;;;
;;;; This file must be used under the terms of the MIT license.
;;;; This source file is licensed as described in the file LICENSE, which
;;;; you should have received as part of this distribution. The terms
;;;; are also available at
;;;; https://opensource.org/licenses/MIT

(in-package #:rashell/test)


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
