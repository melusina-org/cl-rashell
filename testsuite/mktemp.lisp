;;;; mktemp.lisp — Temporary files and directories

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

(rashell:define-utility mkfile (pathname)
  ((mode :option "-m" :to-string (lambda (mode) (format nil "~3,'0O" mode))))
  (:program "/usr/bin/install"
   :documentation "Create an empty file with the given mode."
   :rest (list "/dev/null" pathname)))

(define-testcase test-with-temporary-file/permission (filespec)
  "Assert that the temporary file FILESPEC as at most UNIX permission #o700."
  (assert-t
   (rashell:test
    '(:not
      (:or
       (:has-at-least-permission #o010)
       (:has-at-least-permission #o020)
       (:has-at-least-permission #o040)
       (:has-at-least-permission #o001)
       (:has-at-least-permission #o002)
       (:has-at-least-permission #o004)))
    filespec)))

(define-testcase test-with-temporary-file/write-read (filespec)
  (let ((cookie "A delicious cookie."))
    (with-open-file (s filespec
		       :direction :output
		       :if-exists :supersede
		       :if-does-not-exist :error)
      (write-string cookie s))
    (with-open-file (s filespec
		       :direction :input
		       :if-does-not-exist :error)
      (assert-string= cookie (read-line s)))))

(define-testcase test-with-temporary-file/has-been-removed-p (filespec)
  (unless rashell:*mktemp-keep*
    (assert-nil (rashell:test :prune filespec))))

(define-testcase test-with-temporary-file ()
  "Test the properties of the temporary file."
  (let ((filespec-escaping-scope))
    (rashell:with-temporary-file (filespec)
      (test-with-temporary-file/permission filespec)
      (test-with-temporary-file/write-read filespec)
      (setf filespec-escaping-scope filespec))
    (test-with-temporary-file/has-been-removed-p filespec-escaping-scope)))

(defun test-with-temporary-directory/populate (is-directory-p permission path)
  (if is-directory-p
      (rashell:mkdir path :mode permission)
      (mkfile path :mode permission)))

(define-testcase test-with-temporary-directory ()
  "Test the properties of the temporary directory."
  (let ((spec '((t #o700 "a")
		(t #o750 "a/b")
		(nil #o600 "a/b/x")
		(nil #o640 "a/y")
		(t #o700 "c")
		(nil #o200 "c/z"))))
    (rashell:with-temporary-directory (tmpdir)
      (loop :for item :in spec
	    :do (destructuring-bind (is-directory-p permission path) item
		  (test-with-temporary-directory/populate is-directory-p permission
							  (merge-pathnames path tmpdir))))
      (loop :for item :in spec
	    :do (destructuring-bind (is-directory-p permission path) item
		  (assert-t (rashell:test `(:and
					    (:has-kind ,(if is-directory-p :directory :regular))
					    (:has-exact-permission ,permission))
					  (merge-pathnames path tmpdir))))))))

(define-testcase mktemp-testsuite ()
  "Test Rashell functions built around temporary files and directories."
  (test-with-temporary-file)
  (test-with-temporary-directory))

;;;; End of file `mktemp.lisp'
