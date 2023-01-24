;;;; testsuite.lisp — Testsuite

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

(define-testcase run-all-tests()
  "Run all available tests."
  (util-testsuite)
  (rashell-testsuite)
  (posix-testsuite)
  (mktemp-testsuite))

;;;; End of file `testsuite.lisp'
