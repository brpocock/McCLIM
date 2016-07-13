;;; -*- lisp -*-

(defpackage :clim-sysadmin.system
  (:use :cl :asdf))

(in-package :clim-sysadmin.system)

(defsystem :clim-sysadmin
  :depends-on (:mcclim :oliphaunt)
  :components
  ((:module "Apps/SysAdmin"
            :pathname #.(make-pathname :directory '(:relative "Apps" "SysAdmin"))
            :components
            ((:file "clim-sysadmin")))))

