;;; -*- lisp -*-

(defpackage :clim-sysadmin.system
  (:use :cl :asdf))

(in-package :clim-sysadmin.system)

(defsystem :clim-sysadmin
  :depends-on (:mcclim :oliphaunt)
  :components
  ((:file "clim-sysadmin")))

