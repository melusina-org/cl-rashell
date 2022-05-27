(in-package :rashell-user)

(defvar *find* (find* '(:has-kind :regular) #p"."))

(defun f-1 ()
  (do-query (regular-file *find*)
    (write-line regular-file)))

(defun f-2 ()
  (flet
      ((result ())
       (process-one-regular-file (regular-file)
         (write-line regular-file)))
    (declare (inline result process-one-regular-file))
    (rashell::do-query/loop *find* #'process-one-regular-file #'result)))
