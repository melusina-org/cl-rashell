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

(in-package #:org.melusina.rashell)


;;;;
;;;; File Utilities
;;;;

(defparameter *find-predicate-grammar-table*
  '((:print)
    (:prune)
    (:has-kind 1)
    (:has-suffix 1)
    (:is-owned-by-user 1)
    (:is-owned-by-group 1)
    (:is-newer-than 1)
    (:has-exact-permission 1)
    (:has-at-least-permission 1)
    (:name 1)
    (:path 1)
    (:and expr-*)
    (:or expr-*)
    (:not expr))
  "The grammar table for find(1) predicates.")

(defun find-predicate-to-argv (predicate-form)
  "Convert PREDICATE-FORM to a sequence of arguments for the find(1) utility.

The PREDICATE-FORM expression is a list matching one of the following examples.

  (:PRINT)
  (:PRUNE)
  (:HAS-KIND FILE-KIND)
  (:HAS-SUFFIX SUFFIX)
  (:IS-OWNED-BY-USER USER-NAME-OR-USER-ID)
  (:IS-OWNED-BY-GROUP GROUP-NAME-OR-GROUP-ID)
  (:IS-NEWER-THAN PATH)
  (:HAS-EXACT-PERMISSION MODE)
  (:HAS-AT-LEAST-PERMISSION MODE)
  (:NAME GLOBBING-PATTERN)
  (:PATH GLOBBING-PATTERN)
  (:AND PREDICATE-FORM*)
  (:OR PREDICATE-FORM*)
  (:NOT PREDICATE-FORM)

Valid FILE-KIND paramters are

  :BLOCK-SPECIAL
  :CHARACTER-SPECIAL
  :DIRECTORY
  :SYMBOLIC-LINK
  :FIFO
  :REGULAR
  :SOCKET
"
  (flet ((symbolic-or-numeric-id-to-string (id)
           (cond
             ((stringp id) id)
             ((numberp id) (write-to-string id))
             (t (error "~A: Invalid symbolic or numeric id." id))))
	 (taste (predicate-form)
	   (cond
	     ((symbolp predicate-form)
	      predicate-form)
	     ((null predicate-form)
	      :empty)
	     ((listp predicate-form)
	      (first predicate-form))
	     (t
	      (error "~S: Invalid predicate form." predicate-form)))))
    (case (taste predicate-form)
      (:empty nil)
      (:prune '("-prune"))
      (:print '("-print"))
      (:has-kind
       (list "-type"
             (case (second predicate-form)
               (:block-special "b")
               (:character-special "c")
               (:directory "d")
               (:symbolic-link "l")
               (:fifo "p")
               (:regular "f")
               (:socket "s"))))
      (:has-suffix
       (list "-name" (concatenate 'string "*." (second predicate-form))))
      (:is-owned-by-user
       (list "-user" (symbolic-or-numeric-id-to-string (second predicate-form))))
      (:is-owned-by-group
       (list "-group" (symbolic-or-numeric-id-to-string (second predicate-form))))
      (:is-newer-than
       (list "-newer" (second predicate-form)))
      (:has-exact-permission
       (list "-perm" (write-to-string (second predicate-form) :base 8)))
      (:has-at-least-permission
       (list "-perm" (concatenate 'string "-" (write-to-string (second predicate-form) :base 8))))
      (:name
       (list "-name" (second predicate-form)))
      (:path
       (list "-path" (second predicate-form)))
      (:not
       (concatenate 'list '("-not" "(") (find-predicate-to-argv (second predicate-form)) '(")")))
      ((:or :and)
       (let ((operator (case (first predicate-form)
			 (:or "-or")
			 (:and "-and")))
             answer)
         (do ((tail (mapcar #'find-predicate-to-argv (rest predicate-form)) (rest tail)))
             ((null tail) (nreverse answer))
           (push "(" answer)
           (dolist (arg (first tail)) (push arg answer))
           (push ")" answer)
           (unless (null (rest tail))
             (push operator answer))))))))


(defun command-find (predicate-expr pathname &key directory environment follow)
  "Prepare a find(1) command on PATHNAME with the given PREDICATE-EXPR.

The options are

 FOLLOW
   If set, symbolic links are followed.

 DIRECTORY
    A pathname to a working doirectory to use when running the command.
"
  ;; References: http://pubs.opengroup.org/onlinepubs/9699919799/utilities/find.html
  (make-instance 'command
                 :program #p"/usr/bin/find"
                 :directory directory
                 :environment environment
                 :argv (append
                        (when follow (list "-L"))
                        (mapcar #'define-command/to-string (ensure-list pathname))
                        (find-predicate-to-argv predicate-expr))))

(defun find* (predicate-expr pathname &key directory environment follow)
  (run-query (command-find predicate-expr pathname
			   :directory directory
			   :environment environment
			   :follow follow)))

(defmacro do-find ((var (predicate-expr pathname &key directory environment follow) &optional result) &body body)
  `(do-query (,var (command-find ,predicate-expr ,pathname
				 :directory ,directory
				 :environment ,environment
				 :follow ,follow)
		   ,result)
     ,@body))


(defun test (predicate-expr pathname &key follow directory environment)
  "Test the meta-data of file PATHNAME against PREDICATE-EXPRESSION.
If the file does not exist, the test evaluates to NIL."
  (declare (ignore environment))
  (let (stat)
    (handler-case
        (let ((current
		(when directory
		  (uiop:getcwd))))
          (when directory
	    (uiop:chdir directory))
          (setf stat
		(if follow (sb-posix:lstat pathname) (sb-posix:stat pathname)))
          (when current
	    (uiop:chdir current)))
      (sb-posix:syscall-error (condition)
        (declare (ignore condition))
        (setf stat nil)))
    (labels
        ((taste (predicate-form)
	   (cond
	     ((symbolp predicate-form)
	      predicate-form)
	     ((null predicate-form)
	      :empty)
	     ((listp predicate-form)
	      (first predicate-form))
	     (t
	      (error "~S: Invalid predicate form." predicate-form))))
	 (eval-predicate (predicate-form)
           (case (taste predicate-form)
             ((:prune :print)
	      t)
             (:has-kind
              (let ((mode-mask
                      (ecase (second predicate-form)
                        (:block-special sb-posix:s-ifblk)
                        (:character-special sb-posix:s-ifchr)
                        (:directory sb-posix:s-ifdir)
                        (:symbolic-link sb-posix:s-iflnk)
                        (:fifo sb-posix:s-ififo)
                        (:regular sb-posix:s-ifreg)
                        (:socket sb-posix:s-ifsock))))
                (/= (logand mode-mask (sb-posix:stat-mode stat)) 0)))
             (:has-suffix
              (string-match (concatenate 'string "*." (second predicate-form)) pathname))
             (:is-owned-by-user
              (typecase (second predicate-form)
                (number
                 (eq (second predicate-form) (sb-posix:stat-uid stat)))
                (string
                 (let ((passwd-entry (sb-posix:getpwnam (second predicate-form))))
                   (if passwd-entry
                       (eq (sb-posix:passwd-uid passwd-entry) (sb-posix:stat-uid stat))
                       (error "~A: No such user." (second predicate-form)))))
                (t
                 (error "~S: Invalid user." (second predicate-form)))))
             (:is-newer-than
              (<= (sb-posix:stat-mtime (sb-posix:stat (second predicate-form)))
                  (sb-posix:stat-mtime stat)))
             (:has-exact-permission
              (eq (logandc2 (sb-posix:stat-mode stat) sb-posix:s-ifmt)
		  (logandc2 (second predicate-form) sb-posix:s-ifmt)))
             (:has-at-least-permission
              (eq (logand
		   (logandc2 (sb-posix:stat-mode stat) sb-posix:s-ifmt)
		   (logandc2 (second predicate-form) sb-posix:s-ifmt))
		  (logandc2 (second predicate-form) sb-posix:s-ifmt)))
             (:name
              (or
               (string-match (second predicate-form) pathname)
               (string-match (concatenate 'string "*/" (second predicate-form)) pathname)))
             (:path
              (string-match (second predicate-form) pathname))
             (:not
              (not (eval-predicate (second predicate-form))))
             (:and
              (loop for subform in (rest predicate-form) always (eval-predicate subform)))
             (:or
              (loop for subform in (rest predicate-form) thereis (eval-predicate subform))))))
      (when stat (eval-predicate predicate-expr)))))

(define-utility cp (pathname-list destination)
  ((follow :flag "-H")
   (force :flag "-f")
   (recursive :flag "-R"))
  (:program #p"/bin/cp"
   :documentation "Run cp(1) on PATHNAME-LIST and DESTINATION."
   :reference "http://pubs.opengroup.org/onlinepubs/9699919799/utilities/cp.html"
   :rest (append (ensure-list pathname-list) (list destination))))

(define-utility rm (pathname-list)
  ((force :flag "-f")
   (recursive :flag "-R"))
  (:program #p"/bin/rm"
   :documentation "Run rm(1) on PATHNAME-LIST."
   :reference "http://pubs.opengroup.org/onlinepubs/9699919799/utilities/rm.html"
   :rest (ensure-list pathname-list)))

(define-utility mv (pathname-list destination)
  ((force :flag "-f"))
  (:program "#p/bin/mv"
   :documentation "Run mv(1) on PATHNAME-LIST and DESINATION."
   :reference "http://pubs.opengroup.org/onlinepubs/9699919799/utilities/mv.html"
   :rest (append (ensure-list pathname-list) (list destination))))

(define-utility ln (pathname destination)
  ((force :flag "-f")
   (symbolic :flag "-s"))
  (:program #p"/bin/ln"
   :documentation "Run ln(1) on PATHNAME and DESINATION."
   :reference "http://pubs.opengroup.org/onlinepubs/9699919799/utilities/ln.html"
   :rest (list pathname destination)))

(define-utility mkdir (pathname-list)
  ((mode :option "-m" :to-string (lambda (mode) (format nil "~3,'0O" mode)))
   (create-intermediate :flag "-p"))
  (:program #p"/bin/mkdir"
   :documentation "Run mkdir(1) on PATHNAME."
   :reference "http://pubs.opengroup.org/onlinepubs/9699919799/utilities/mkdir.html"
   :rest (ensure-list pathname-list)))

(define-query cat (pathname-list)
  nil
  (:program #p"/bin/cat"
   :documentation "Run cat(1) on PATHNAME-LIST."
   :reference "http://pubs.opengroup.org/onlinepubs/9699919799/utilities/cat.html"
   :rest (ensure-list pathname-list)))

(define-filter sed (sedscript)
  ((echo :flag "-n"))
  (:program #p"/usr/bin/sed"
   :documentation "Run sed(1) with the given SEDSCRIPT on INPUT."
   :reference "http://pubs.opengroup.org/onlinepubs/9699919799/utilities/sed.html"
   :rest (list "-e" sedscript)))

(define-filter awk (awkscript)
  ((sepstring :option "-F")
   (assignment :option "-v"
	       :multiple t
	       :to-string (lambda (x) (concatenate 'string (first x) "=" (second x)))))
  (:program #p"/usr/bin/awk"
   :reference "http://pubs.opengroup.org/onlinepubs/9699919799/utilities/awk.html"
   :documentation "Run awk(1) with the given AWKSCRIPT on INPUT."
   :rest (list awkscript)))

(defstruct free-disk-space
  (device nil :type string)
  (blocks 0 :type number)
  (used 0 :type number)
  (free 0 :type number)
  (capacity 0.0 :type float)
  (mounted-on nil :type string))

(defparameter *free-disk-space-scanner*
  (ppcre:create-scanner "^(.*[^ ]) +([0-9]+) +([0-9]+) +([0-9]+) +([^ ]*)% +(.*)$")
  "The PPCRE scanner used to scan disk free space records.")

(defun free-disk-space-of-string (text)
  "Read FREE-DISK-SPACE from the output of DF."
  (multiple-value-bind (ignored-text registers)
      (ppcre:scan-to-strings *free-disk-space-scanner* text)
    (declare (ignore ignored-text))
    (make-free-disk-space
     :device (aref registers 0)
     :blocks (parse-integer (aref registers 1))
     :used (parse-integer (aref registers 2))
     :free (parse-integer (aref registers 3))
     :capacity (parse-float (aref registers 4))
     :mounted-on (aref registers 5))))

(define-query df (path-or-paths)
  nil
  (:program #p"/bin/df"
   :reference "http://pubs.opengroup.org/onlinepubs/9699919799/utilities/df.html"
   :documentation "Return the available free disk space on the devices for
the given PATHS, or for all devices if the empty list is given.  The
free disk space is computed with `df -k -P` as described in df(1)."
   :rest (list* "-k" "-P" (if (consp path-or-paths) path-or-paths (list path-or-paths)))
   :object-of-output-line
            (lambda (output-line)
              (if (>= *query-output-line-number* 2)
                  (free-disk-space-of-string output-line)
                  (values :drop)))))

(defparameter *consumed-disk-space-scanner*
  (ppcre:create-scanner "^([0-9]+)[ 	]+(.+)$")
  "The PPCRE scanner used to scan consumed space records.")

(defun consumed-disk-space-of-string (text)
  "Read FREE-DISK-SPACE from the output of DF."
  (multiple-value-bind (ignored-text registers)
      (ppcre:scan-to-strings *consumed-disk-space-scanner* text)
    (declare (ignore ignored-text))
    (list (aref registers 1) (parse-integer (aref registers 0)))))

(define-query du (pathname)
  nil
  (:program #p"/usr/bin/du"
   :documentation "Query the consumed disk space for the file hierarchies
rooted at the given PATHNAME.

The output is an alist mapping PATHS to consumed disk space in kilobytes."
   :rest (list* "-s" "-k" (ensure-list pathname))
   :object-of-output-line #'consumed-disk-space-of-string))

;;;; End of file `posix.lisp'
