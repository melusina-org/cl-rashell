;;;; package.lisp — Resilient replicant Shell Programming Library for Common Lisp

;;;; Rashell (https://github.com/melusina-org/cl-rashell)
;;;; This file is part of Rashell.
;;;;
;;;; Copyright © 2017–2023 Michaël Le Barbier
;;;; All rights reserved.

;;;; This file must be used under the terms of the MIT License.
;;;; This source file is licensed as described in the file LICENSE, which
;;;; you should have received as part of this distribution. The terms
;;;; are also available at https://opensource.org/licenses/MIT

(defpackage #:rashell
  (:use #:common-lisp)
  (:import-from #:parse-float
   #:parse-float)
  (:import-from #:alexandria
   #:make-keyword
   #:with-unique-names)
  (:export
   #:command
   #:make-command
   #:define-command
   #:command-p
   #:command-input
   #:command-output
   #:command-error
   #:command-status
   #:run-command
   #:wait-command
   #:kill-command
   #:close-command
   #:arranged-conversation
   #:run-utility
   #:define-utility
   #:run-test
   #:define-test
   #:run-query
   #:do-query
   #:define-query
   #:run-filter
   #:do-filter 
   #:define-filter
   #:*query-output-line-number*

   ;; POSIX - File Utilities
   #:command-find
   #:find*
   #:do-find
   #:test
   #:cp
   #:command-cp
   #:rm
   #:command-rm
   #:mv
   #:command-mv
   #:ln
   #:command-ln
   #:mkdir
   #:command-mkdir
   #:cat
   #:command-cat

   ;; POSIX – Sed & Awk
   #:sed
   #:command-sed
   #:do-sed
   
   #:awk
   #:command-awk
   #:do-awk

   #:cat
   #:command-cat
   #:do-cat
   
   ;; POSIX – Free Disk Space
   #:free-disk-space
   #:free-disk-space-device
   #:free-disk-space-blocks
   #:free-disk-space-used
   #:free-disk-space-free
   #:free-disk-space-capacity
   #:free-disk-space-mounted-on
   #:df
   #:command-df
   #:du
   #:command-du

   ;; Temporary Files and Directory
   #:*mktemp-keep*
   #:*mktemp-designator*
   #:command-make-temporary-file
   #:command-make-temporary-directory
   #:make-temporary-file
   #:make-temporary-directory
   #:with-temporary-file
   #:with-temporary-directory
   
   ))

;;;; End of file `package.lisp'
