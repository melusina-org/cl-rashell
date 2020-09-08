;;;; rashell.lisp – Resilient replicant Shell Programming Library for Common Lisp

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

(rashell:define-command test/cp (pathname-list destination)
  ((follow :flag "-H")
   (force :flag "-f")
   (recursive :flag "-R"))
  (:program "/bin/cp"
   :documentation "Run cp(1) on PATHNAME-LIST and DESTINATION."
   :reference "http://pubs.opengroup.org/onlinepubs/9699919799/utilities/cp.html"
   :argv-rest (append (rashell::ensure-list pathname-list) (list destination))))

(rashell:define-command test/mkdir (pathname-list)
  ((mode :option "-m" :to-string (lambda (mode) (format nil "~3,'0O" mode)))
   (create-intermediate :flag "-p"))
  (:program "/bin/mkdir"
   :documentation "Run mkdir(1) on PATHNAME."
   :reference "http://pubs.opengroup.org/onlinepubs/9699919799/utilities/mkdir.html"
   :argv-rest (rashell::ensure-list pathname-list)))

(define-testcase test-define-command/baseline ()
  (let ((cp (test/cp '(#p"/dev/null") #p"/nonexistant" :workdir #p"/" :force t :recursive t)))
    (assert-equal
     (slot-value cp 'rashell::argv)
     '("-R" "-f" "/dev/null" "/nonexistant"))))

(define-testcase test-define-command/option-to-string ()
  (let ((mkdir (test/mkdir #p"/nonexistant" :mode #O755)))
    (assert-equal
     (slot-value mkdir 'rashell::argv)
     '("-m" "755" "/nonexistant"))))

(define-testcase rashell-testsuite()
  "Run tests for the rashell module."
  (test-define-command/baseline)
  (test-define-command/option-to-string))

;;;; End of file `rashell.lisp'
