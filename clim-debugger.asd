;;; -*- lisp -*-

(defpackage :clim-debugger.system
  (:use :cl :asdf))

(in-package :clim-debugger.system)

(defsystem :clim-debugger
  :depends-on (:mcclim)
  :components
  ((:module "Apps/Debugger"
            :pathname #.(make-pathname :directory '(:relative "Apps" "Debugger"))
            :components
            ((:file "clim-debugger")))))

