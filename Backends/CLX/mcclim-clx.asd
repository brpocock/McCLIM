
(defsystem #:mcclim-clx
  :depends-on (#:clim
               #+(or cmu ecl) (:require :clx)
               #+(or sbcl clozure ecl clisp allegro) #:clx)
  :components
  ((:file "package")
   (:file "image" :depends-on ("package"))
   (:file "keysyms-common" :depends-on ("package"))
   (:file "keysyms" :depends-on ("keysyms-common" "package"))
   (:file "keysymdef" :depends-on ("keysyms-common" "package"))
   (:file "port" :depends-on ("keysyms-common" "keysyms" "package"))
   (:file "medium" :depends-on ("port" "keysyms" "package"))
   (:file "graft" :depends-on ("port" "package"))
   (:file "frame-manager" :depends-on ("medium" "port" "package"))))

(defsystem #:mcclim-clx/pretty
  :depends-on (#:mcclim-clx
               ;; FIXME: truetype is slow, acceptable only on SBCL
               #+sbcl #:mcclim-fonts/truetype
               #:mcclim-looks/pixie))
