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

(rashell:define-command cp (pathname-list destination)
  ((follow :flag "-H")
   (force :flag "-f")
   (recursive :flag "-R"))
  (:program "/bin/cp"
   :documentation "Run cp(1) on PATHNAME-LIST and DESTINATION."
   :reference "http://pubs.opengroup.org/onlinepubs/9699919799/utilities/cp.html"
   :rest (append (rashell::ensure-list pathname-list) (list destination))))

(rashell:define-command mkdir (pathname-list)
  ((mode :option "-m" :to-string (lambda (mode) (format nil "~3,'0O" mode)))
   (create-intermediate :flag "-p"))
  (:program "/bin/mkdir"
   :documentation "Run mkdir(1) on PATHNAME."
   :reference "http://pubs.opengroup.org/onlinepubs/9699919799/utilities/mkdir.html"
   :rest (rashell::ensure-list pathname-list)))

(rashell:define-command tr (translate-from translate-to)
  nil
  (:program #P"/usr/bin/tr" :rest (list translate-from translate-to)))

(define-testcase test-define-command/baseline ()
  (let ((cp (cp '(#p"/dev/null") #p"/nonexistant" :directory #p"/" :force t :recursive t)))
    (assert-equal
     (slot-value cp 'rashell::argv)
     '("-R" "-f" "/dev/null" "/nonexistant"))))

(define-testcase test-define-command/option-to-string ()
  (let ((mkdir (mkdir #p"/nonexistant" :mode #O755)))
    (assert-equal
     (slot-value mkdir 'rashell::argv)
     '("-m" "755" "/nonexistant"))))

(define-testcase test-arranged-conversation/baseline ()
  (let ((arranged-conversation
          (rashell:arranged-conversation
           '((:write-output-line "EHLO")
             (:read-input-line "USERNAME email@invalid.org")
             (:read-input-line "PASSWORD NotVerySecret")
             (:write-output-line "WELCOME email@invalid.org")
             (:exit 0)))))
    (unwind-protect
         (progn
           (rashell:run-command arranged-conversation :input :stream :output :stream :error :stream)
           (assert-string= "EHLO"
                           (read-line (rashell:command-output arranged-conversation)))
           (write-line "USERNAME email@invalid.org" (rashell:command-input arranged-conversation))
           (finish-output (rashell:command-input arranged-conversation))
           (write-line "PASSWORD NotVerySecret" (rashell:command-input arranged-conversation))
           (finish-output (rashell:command-input arranged-conversation))
           (assert-string= "WELCOME email@invalid.org"
                           (read-line (rashell:command-output arranged-conversation)))
           (rashell:wait-command arranged-conversation)
           (assert-eql :EXITED (rashell:command-status arranged-conversation))
           (assert-eql 0 (nth-value 1 (rashell:command-status arranged-conversation))))
      (rashell:kill-command arranged-conversation :kill))))

(define-testcase test-run-tool/baseline ()
  (let ((arranged-conversation
          (rashell:arranged-conversation
           '((:write-output-line "Arranged output.")
             (:exit 0)))))
    (multiple-value-bind (accumulated-output accumulated-error)
        (rashell:run-tool arranged-conversation :trim t)
      (assert-string= "Arranged output." accumulated-output)
      (assert-string= "" accumulated-error)
      (assert-eql :EXITED (rashell:command-status arranged-conversation))
      (assert-eql 0 (nth-value 1 (rashell:command-status arranged-conversation))))))

(define-testcase test-run-test/baseline ()
  (let ((arranged-conversation
          (rashell:arranged-conversation
           '((:write-output-line "Arranged output.")
             (:exit 0)))))
    (multiple-value-bind (test-result accumulated-output accumulated-error)
        (rashell:run-test arranged-conversation)
      (assert-t test-result)
      (assert-string= "Arranged output." (string-trim '(#\Newline) accumulated-output))
      (assert-string= "" accumulated-error)
      (assert-eql :EXITED (rashell:command-status arranged-conversation))
      (assert-eql 0 (nth-value 1 (rashell:command-status arranged-conversation))))))

(define-testcase test-run-query/baseline ()
  (let ((arranged-conversation
          (rashell:arranged-conversation
           '((:write-output-line "A")
             (:write-output-line "B")
             (:exit 0)))))
    (let ((lines (rashell:run-query arranged-conversation)))
      (assert= 2 (length lines))
      (assert-string= "A" (first lines))
      (assert-string= "B" (second lines))
      (assert-eql :EXITED (rashell:command-status arranged-conversation))
      (assert-eql 0 (nth-value 1 (rashell:command-status arranged-conversation))))))

(define-testcase test-run-query/object-of-output-line ()
  (let ((arranged-conversation
          (rashell:arranged-conversation
           '((:write-output-line "Head")
             (:write-output-line "A")
             (:write-output-line "B")
             (:exit 0)))))
    (setf (slot-value arranged-conversation 'rashell::object-of-output-line)
          (lambda (output-line)
            (if (= rashell:*query-output-line-number* 1) :DROP output-line)))
    (let ((lines (rashell:run-query arranged-conversation)))
      (assert= 2 (length lines))
      (assert-string= "A" (first lines))
      (assert-string= "B" (second lines))
      (assert-eql :EXITED (rashell:command-status arranged-conversation))
      (assert-eql 0 (nth-value 1 (rashell:command-status arranged-conversation))))))

(define-testcase test-run-filter/string ()
  (let* ((command (tr "AB" "ab"))
         (output (rashell:run-filter command "ABRACADABRA")))
    (assert-type output 'string)
    (assert-string= "abRaCaDabRa" output)
    (assert-eql :EXITED (rashell:command-status command))
    (assert-eql 0 (nth-value 1 (rashell:command-status command)))))

(define-testcase test-run-filter/multiline-string ()
  (let* ((command (tr "AB" "ab"))
         (output (rashell:run-filter command "ABRACA
DABRA")))
    (assert-type output 'string)
    (assert-string= "abRaCa
DabRa" output)
    (assert-eql :EXITED (rashell:command-status command))
    (assert-eql 0 (nth-value 1 (rashell:command-status command)))))

(define-testcase test-run-filter/nontrimmed-string ()
  (let* ((command (tr "AB" "ab"))
         (output (rashell:run-filter command "ABRACADABRA
")))
    (assert-type output 'string)
    (assert-string= "abRaCaDabRa
" output)
    (assert-eql :EXITED (rashell:command-status command))
    (assert-eql 0 (nth-value 1 (rashell:command-status command)))))

(define-testcase test-run-filter/list ()
  (let* ((command (tr "AB" "ab"))
         (output (rashell:run-filter command '("ABRACADABRA"))))
    (assert-type output 'list)
    (assert= 1 (length output))
    (assert-string= "abRaCaDabRa" (first output))
    (assert-eql :EXITED (rashell:command-status command))
    (assert-eql 0 (nth-value 1 (rashell:command-status command)))))

(define-testcase test-run-filter/array ()
  (let* ((command (tr "AB" "ab"))
         (output (rashell:run-filter command #("ABRACADABRA"))))
    (assert-type output 'array)
    (assert= 1 (length output))
    (assert-string= "abRaCaDabRa" (aref output 0))
    (assert-eql :EXITED (rashell:command-status command))
    (assert-eql 0 (nth-value 1 (rashell:command-status command)))))

(define-testcase rashell-testsuite ()
  "Run tests for the rashell module."
  (test-define-command/baseline)
  (test-define-command/option-to-string)
  (test-arranged-conversation/baseline)
  (test-run-tool/baseline)
  (test-run-test/baseline)
  (test-run-query/baseline)
  (test-run-query/object-of-output-line)
  (test-run-filter/string)
  (test-run-filter/multiline-string)
  (test-run-filter/nontrimmed-string)
  (test-run-filter/list)
  (test-run-filter/array)) 

;;;; End of file `rashell.lisp'
