;;;; setup.lisp — Setup for Rashell

;;;; Rashell (https://github.com/melusina-org/cl-rashell)
;;;; This file is part of Rashell.
;;;;
;;;; Copyright © 2017–2022 Michaël Le Barbier
;;;; All rights reserved.

;;;; This file must be used under the terms of the MIT License.
;;;; This source file is licensed as described in the file LICENSE, which
;;;; you should have received as part of this distribution. The terms
;;;; are also available at https://opensource.org/licenses/MIT

(in-package #:cl-user)

;;;;
;;;; Atelier
;;;;

(ql:quickload "org.melusina.atelier" :silent t)

(org.melusina.atelier:initialise)

(setf org.melusina.atelier:*parameter-bindings*
      '((:copyright-holder . "Michaël Le Barbier")
 	(:copyright-year . "2017–2022")
 	(:project-name . "Rashell")
 	(:project-filename . "org.melusina.rashell")
 	(:project-description . "Resilient replicant Shell Programming Library for Common Lisp")
 	(:homepage . "https://github.com/melusina-org/cl-rashell")
 	(:license . :mit)))

;;;; End of file `setup.lisp'
