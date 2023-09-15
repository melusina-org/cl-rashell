;;;; posix.lisp — Posix utilities

;;;; Rashell (https://github.com/melusina-org/cl-rashell)
;;;; This file is part of Rashell.
;;;;
;;;; Copyright © 2017–2023 Michaël Le Barbier
;;;; All rights reserved.

;;;; This file must be used under the terms of the MIT License.
;;;; This source file is licensed as described in the file LICENSE, which
;;;; you should have received as part of this distribution. The terms
;;;; are also available at https://opensource.org/licenses/MIT

(in-package #:org.melusina.rashell/testsuite)


;;;;
;;;; Rashell POSIX Interface
;;;;

(define-testcase testsuite-find-expr ()
  (assert-equal
   '("(" "-not" "(" "-name" "*.c" ")" ")" "-and" "(" "-print" ")")
   (rashell::find-predicate-to-argv
    '(:and (:not (:name "*.c")) :print))))

(define-testcase testsuite-test ()
  (rashell:with-temporary-file (oldfile)
    (rashell:with-temporary-file (newfile)
      (sb-posix:chmod newfile #o755)
      (assert-t (rashell:test
		 '(:has-kind :regular) oldfile))
      (assert-t (rashell:test
		 `(:is-newer-than ,oldfile) newfile))
      (assert-t (rashell:test
		 '(:has-exact-permission #o755) newfile))
      (assert-t (rashell:test
		 '(:has-at-least-permission #o755) newfile))
      (assert-t (rashell:test
		 '(:has-at-least-permission #o700) newfile))
      (assert-t (rashell:test
		 `(:and
		   (:has-kind :regular) 
		   (:is-newer-than ,oldfile)
		   (:has-at-least-permission #o755))
		 newfile))
      (assert-nil (rashell:test
		   `(:and
		     (:has-kind :regular) 
		     (:is-newer-than ,newfile)
		     (:has-at-least-permission #o755))
		   oldfile))
      (assert-t (rashell:test
		   `(:or
		     (:has-kind :regular) 
		     (:is-newer-than ,newfile)
		     (:has-at-least-permission #o755))
		   oldfile)))))

(define-testcase posix-testsuite ()
  "Run tests for the posix module."
  (testsuite-find-expr)
  (testsuite-test))

;;;; End of file `posix.lisp'
