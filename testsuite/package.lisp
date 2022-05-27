;;;; package.lisp — Resilient replicant Shell Programming Library for Common Lisp

;;;; Rashell (https://github.com/melusina-org/cl-rashell)
;;;; This file is part of Rashell.
;;;;
;;;; Copyright © 2017–2022 Michaël Le Barbier
;;;; All rights reserved.

;;;; This file must be used under the terms of the MIT License.
;;;; This source file is licensed as described in the file LICENSE, which
;;;; you should have received as part of this distribution. The terms
;;;; are also available at https://opensource.org/licenses/MIT

(defpackage #:org.melusina.rashell/testsuite
  (:local-nicknames (#:rashell #:org.melusina.rashell))
  (:use #:common-lisp)
  (:import-from #:org.melusina.confidence
   #:define-testcase
   #:assert=
   #:assert-eql
   #:assert-equal
   #:assert-nil
   #:assert-string-match
   #:assert-string=
   #:assert-t
   #:assert-type
   ))

;;;; End of file `package.lisp'
