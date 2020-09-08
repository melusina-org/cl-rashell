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

;;;;
;;;; The COMMAND class
;;;;

(in-package #:rashell)

(defclass command ()
  ((program
    :initarg :program
    :initform #p"/usr/bin/false"
    :documentation 
    "A path to the program to run.")
   (argv
    :initarg :argv
    :initform nil
    :documentation
    "A sequence to be used as the argument vector for the program.")
   (workdir
    :initarg :workdir
    :initform nil
    :documentation
    "The working directory of the program to run.
If not provided, the current working directory is used.")
   (environment
    :initarg :environment
    :initform nil
    :documentation
    "Environment variable bindings for the program to run.
The ENVIRONMENT slot either describes environment bindings to be
added to the current process environment, or the exhaustive list
of environment bindings when the slot REPLACE-ENVIRONMENT-P
is set to T.

The ENVIRONMENT must be a sequence whose terms are:
 - either a string of the form \"VARIABLE=VALUE\";
 - or a cons cell of the form (VARIABLE . VALUE).")
   (replace-environment-p
    :initarg :replace-environment-p
    :initform nil
    :documentation
    "Flag controlling the interpretation of the ENVIRONMENT slot.")
   (external-program
    :initform nil
    :documentation
    "The instance of EXTERNAL-PROGRAM running the program."))
  (:documentation
   "The COMMAND structure captures the parameters used to start an external program."))


;;;;
;;;; The DEFINE-COMMAND Macro
;;;;

(defun define-command/to-string (argument)
  "Convert ARGUMENT to a string."
  (typecase argument
    (string
     argument)
    (keyword
     (symbol-name argument))
    (pathname
     (namestring argument))
    (t
     (write-to-string argument))))

(defun define-command/prepare-argv (argument spec)
  "Prepare argument vector interpreation fragment for ARGUMENT interpreted according to SPEC."
  (let ((option (getf spec :option))
        (flag (getf spec :flag))
        (to-string (getf spec :to-string)))
    (cond
      (flag
       `(when ,argument (list ,flag)))
      ((and option to-string (getf spec :multiple))
       `(loop for single-argument in (ensure-list ,argument)
              collect ,option
              collect (funcall ,to-string single-argument)))
      ((and option (getf spec :multiple))
       `(loop for single-argument in (ensure-list ,argument)
              collect ,option
              collect single-argument))
      ((and option to-string (not (getf spec :multiple)))
       `(when ,argument
          (list ,option (funcall ,to-string ,argument))))
      ((and option (not (getf spec :multiple)))
       `(when ,argument
          (list ,option ,argument)))
      (t
       (error "~S: Cannot prepare argument vector according to SPEC." spec)))))


(defmacro define-command (name argv options spec)
  "Define a function NAME that can run a command according to SPEC.

The function NAME accepts arguments ARGV and optional arguments as specified by the OPTIONS
parameter, see below.  The SPEC parameter is a property list specifiying various aspects of how the
command is run.

The OPTIONS parameter is a list of option specifications. An option specification is a list
starting with a symbol, the OPTION-NAME, which is used to label the optional parameter of
the function NAME. The allowed forms for option specifications are:

  '(OPTION-NAME :flag FLAG-STRING)
    The parameter OPTION-NAME is interpreted as a generalised boolean.  When it is set, the
    FLAG-STRING is added to the command-lin of the external program being run.

  '(OPTION-NAME :option OPTION-STRING [:to-string CONVERT] [:multiple MULTIPLE-FLAG])
    The parameter OPTION-NAME is interpreted as an arbitrary value is a string, or is converted to a
    string either by applying the function passed as the :to-string property, 
    or by using `write-to-string' if none of the preceeding rules apply.

    When set, the MULTIPLE-FLAG makes the OPTION-NAME accept a list or a single value. 
    The elements of this list are converted to strings as described above and each of
    the resulting string is added to the command line, preceded by OPTION-STRING.


The SPEC is a property list where the following properties are allowed:

  :program PATH-TO-PROGRAM
    The path to the program run by the function NAME.

  :reference
    A reference to be added to the documentation.

  :documentation
    A documentation string for NAME.

  :argv-rest
    A form to evalute in order to produce remaining arguments on the command line."
  (let ((docstring
          (or (getf spec :documentation)
              (format nil "I am too lazy to write documentation for ~A." name)))
        (defun-argv
          (concatenate 'list argv '(&key) '(workdir environment replace-environment-p) (mapcar #'first options)))
        (program
          (getf spec :program))
        (prepare-argv-body
          (let ((argv-rest (getf spec :argv-rest)))
            (cond
              ((eq (first argv-rest) 'append)
               (rest argv-rest))
              (t
               (list argv-rest))))))
    (dolist (option options)
      (push (define-command/prepare-argv (first option) (rest option)) prepare-argv-body))
    `(defun ,name (,@defun-argv)
       ,docstring
       (let ((command-argv
               (mapcar #'define-command/to-string (append ,@prepare-argv-body))))
         (make-instance 'command
                        :program ,program
                        :argv command-argv
                        :workdir workdir
                        :environment environment
                        :replace-environment-p replace-environment-p)))))

;;;; End of file `rashell.lisp'
