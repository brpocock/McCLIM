;;; (c) copyright 2004 by Peter Mechlenborg (metch@daimi.au.dk)
;;; Modified by Bruce-Robert Fenn Pocock (mostly for æsthetics)


;;; This library is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU Library General Public
;;; License as published by the Free Software Foundation; either
;;; version 2 of the License, or (at your option) any later version.
;;;
;;; This library is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; Library General Public License for more details.
;;;
;;; You should have received a copy of the GNU Library General Public
;;; License along with this library; if not, write to the
;;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;;; Boston, MA  02111-1307  USA.


;;; This is the beginning of a Common Lisp debugger implemented in
;;; McCLIM. It uses the portable debugger interface developed for the
;;; Slime project, and the graphical layout is also heavily inspired
;;; by Slime. Because of Slime I hope that this works on other
;;; implementations than SBCL.



;;;
;;; Test:
;;;
;;; For at quick test, you can use this code snippet:
;;;
;;; (let ((*debugger-hook* #'clim-debugger:debugger))
;;;   (+ 3 'abc))
;;;
;;; This is also nice :-)
;;;
;;; (let ((*debugger-hook* #'clim-debugger:debugger))
;;;   (clim-listener:run-listener :new-process t))

;;;
;;; Problems/todo:
;;;
;;; - Elliott Johnson is to be thanked for the nice scroll-bars, but
;;;   for some reason they don't remember their position when clicking
;;;   on a stack-frame or "more".
;;;
;;; - The break function does not use the clim-debugger --> Christophe
;;;   Rhodes was kind enough to inform me that on SBCL,
;;;   SB-EXT:*INVOKE-DEBUGGER-HOOK* takes care off this problem. I
;;;   still don't know if this is a problem with other compilers.
;;;
;;; - "Eval in frame" is not supported. I don't know of a good way to
;;;   do this currently.
;;;
;;; - Goto source location is not supported, but I think this could be
;;;   done through slime.
;;;
;;; - Currently the restart chosen by the clim-debugger is returned
;;;   through the global variable *returned-restart*, this is not the
;;;   best solution, but I do not know how of a better way to return a
;;;   value from a clim frame, when it exits.
;;;
;;; - There need to  be added keyboard shortcuts. 'q'  should exit the debugger with  an abort. '0',
;;;   '1' and so forth should activate the restarts,  like Slime. Maybe is should be possible to use
;;;   the  arrow keys  as well.  Then we  have to  add a  notion of  the current  frame. Would  this
;;;   be useful?
;;;


(defpackage "CLIM-DEBUGGER"
  (:use  "CL-USER" "CLIM" "CLIM-LISP")
  (:export :debugger))

(in-package :clim-debugger)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;   Misc   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Borrowed from Andy Hefner
(defmacro bold ((stream) &body body)
  `(with-text-face (,stream :bold)
     ,@body))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;   Data model    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass debugger-info ()
  ((the-condition :accessor the-condition
                  :initarg :the-condition)
   (condition-message :accessor condition-message
                      :initarg  :condition-message)
   (type-of-condition :accessor type-of-condition
                      :initarg  :type-of-condition)
   (condition-extra :accessor condition-extra
                    :initarg  :condition-extra)
   (restarts :accessor restarts
             :initarg :restarts)
   (backtrace :accessor backtrace
              :initarg :backtrace)))

(defclass minimized-stack-frame-view (textual-view)())
(defclass maximized-stack-frame-view (textual-view)())

(defparameter +minimized-stack-frame-view+ 
  (make-instance 'minimized-stack-frame-view))
(defparameter +maximized-stack-frame-view+ 
  (make-instance 'maximized-stack-frame-view))

(defclass stack-frame ()
  ((clim-view       :accessor view :initform +minimized-stack-frame-view+)
   (frame-string    :accessor frame-string
                    :initarg  :frame-string)
   (frame-no        :accessor frame-no
                    :initarg :frame-no)
   (frame-variables :accessor frame-variables
                    :initarg :frame-variables)))

(defun compute-backtrace (start end)
  (loop for frame in (swank-backend::compute-backtrace start end)
     for frame-no from 0
     collect (make-instance
              'stack-frame
              :frame-string (let ((*print-pretty* nil))
                              (with-output-to-string (stream) 
                                (swank-backend::print-frame frame stream)))
              :frame-no frame-no
              :frame-variables (swank-backend::frame-locals frame-no))))

(defmethod expand-backtrace ((info debugger-info) (value integer))
  (with-slots (backtrace) info
    (setf backtrace (compute-backtrace 0 (+ (length backtrace) 10)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;   CLIM stuff   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass debugger-pane (application-pane)
  ((condition-info :reader condition-info :initarg :condition-info)))

(defun make-debugger-pane (condition)
  (with-look-and-feel-realization ((frame-manager *application-frame*)
                                   *application-frame*) 
    (make-pane 'debugger-pane 
               :condition-info condition
               :display-function #'display-debugger
               :end-of-line-action :allow
               :end-of-page-action :scroll
               :width 800)))

(define-application-frame clim-debugger ()
  ((condition-info :initarg :condition-info
                   :reader clim-condition-info)
   (me-or-my-encapsulation :initarg :debugger 
                           :reader clim-debugger-or-encapsulation))
  (:panes (debugger-pane (make-debugger-pane 
                          (clim-condition-info *application-frame*))))
  (:layouts (default (vertically () (scrolling () debugger-pane))))
  (:geometry :height 600 :width 800))

(defun run-debugger-frame (condition-info me-or-my-encapsulation)
  (run-frame-top-level
   (make-application-frame 'clim-debugger :condition-info condition-info
                           :debugger me-or-my-encapsulation)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;   Presentation types   ;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-presentation-type stack-frame () :inherit-from 't)
(define-presentation-type restart     ())
(define-presentation-type more-type   ())
(define-presentation-type inspect     ())


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;   Commands   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-clim-debugger-command (com-more :name "More backtraces")
    ((pane 'more-type))
  (expand-backtrace (condition-info pane) 10))

(define-clim-debugger-command (com-invoke-inspector :name "Invoke inspector")
    ((obj 'inspect))
  (clouseau:inspector obj))

(define-clim-debugger-command (com-refresh :name "Refresh" :menu t) ()
  (change-space-requirements (frame-panes *application-frame*)))

(define-clim-debugger-command (com-quit :name "Quit" :menu t) ()
  (frame-exit *application-frame*))

(define-clim-debugger-command (com-invoke-restart :name "Invoke restart")
    ((restart 'restart))
  (debugger-restart restart 
                    (clim-debugger-or-encapsulation *application-frame*))
  (frame-exit *application-frame*))

(define-clim-debugger-command (com-toggle-stack-frame-view 
                               :name "Toggle stack frame view")
    ((stack-frame 'stack-frame))
  (progn
    (clouseau:inspector stack-frame)
    (if (and (eq +minimized-stack-frame-view+ (view stack-frame))
             (frame-variables stack-frame))
        (setf (view stack-frame) +maximized-stack-frame-view+)
        (setf (view stack-frame) +minimized-stack-frame-view+))
    (change-space-requirements (frame-panes *application-frame*))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;   Command translators   ;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-presentation-to-command-translator more-backtraces
    (more-type com-more clim-debugger :gesture :select)
  (object)
  (list object))

(define-presentation-to-command-translator invoke-inspector
    (inspect com-invoke-inspector clim-debugger :gesture :select)
    (object)
  (list object))

(define-presentation-to-command-translator toggle-stack-frame-view
    (stack-frame com-toggle-stack-frame-view clim-debugger :gesture :select)
    (object)
  (list object))

(define-presentation-to-command-translator invoke-restart
    (restart com-invoke-restart clim-debugger :gesture :select)
  (object)
  (list object))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;   Display debugging info   ;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun std-form (pane first second &key (family :serif))
  (formatting-row 
   (pane)
   (with-text-family (pane :sans-serif)
     (formatting-cell (pane) (bold (pane) (format t "~A" first))))
   (formatting-cell (pane)
                    (with-text-family (pane family) 
                      (filling-output (pane :fill-width 600)
                                      (format pane "~A" second))))))

(defun present-restart (pane r)
  (formatting-row (pane)
    (with-output-as-presentation (pane r 'restart)
      (formatting-cell (pane)
        (with-text-size (pane :large)
          (with-text-family (pane :sans-serif)
            (format pane "[ ~:(~A~) ]" (restart-name r))))) 
      (formatting-cell (pane)
        (with-text-family (pane :serif)
          (filling-output (pane :fill-width 600)
            (format pane "~A" r)))))))

(defun display-debugger (frame pane)
  (let ((*standard-output* pane))
    (formatting-table (pane)
      (std-form pane "Condition type:" (type-of-condition (condition-info pane))) 
      (formatting-row (pane)
        (formatting-cell (pane)
          (with-text-face (pane :bold)
            (princ "Condition:" pane)))
        (formatting-cell (pane)
          (present (the-condition (condition-info pane)) 'inspect)))
      (formatting-row (pane)
        (formatting-cell (pane)
          (with-text-face (pane :bold)
            (princ "Description:" pane)))
        (formatting-cell (pane) 
          (with-text-size (pane :very-large) 
            (with-text-family (pane :serif)
              (princ (or (condition-message (condition-info pane))
                         (format pane "~a" (the-condition (condition-info pane))))
                     pane)))))
      (when (condition-extra (condition-info pane))
        (std-form pane "Extra:" (condition-extra (condition-info pane))
                  :family :fix)))
    (fresh-line)
    
    (with-text-family (pane :sans-serif)
      (with-text-size (pane :very-large)
        (with-text-face (pane :bold)
          (format t "Restarts:"))))
    (fresh-line)
    (format t " ")
    (formatting-table (pane)
      (loop for restart in (restarts (condition-info pane))
         do (present-restart pane restart)))
    (fresh-line)
    (display-backtrace frame pane)
    (change-space-requirements 
     pane
     :width (bounding-rectangle-width (stream-output-history pane))
     :height (bounding-rectangle-height (stream-output-history pane)))))


(defun display-backtrace (frame pane)
  (declare (ignore frame)) 
  (with-text-family (pane :sans-serif)
    (with-text-size (pane :very-large)
      (with-text-face (pane :bold)
        (format t "Backtrace:"))))
  (fresh-line)
  (format t " ")
  (formatting-table (pane)
    (with-text-family (pane :serif)
      (loop for stack-frame in (backtrace (condition-info pane))
         for i from 0
         do (formatting-row (pane)
              (with-output-as-presentation (pane stack-frame 'stack-frame)
                (with-text-face (pane :bold)
                  (formatting-cell (pane)
                    (format t "~A: " i)))
                (formatting-cell (pane)
                  (present stack-frame 'stack-frame 
                           :view (view stack-frame)))))))
    (when (>= (length (backtrace (condition-info pane))) 20)
      (formatting-row (pane)
        (formatting-cell (pane))
        (formatting-cell (pane)
          (bold (pane)
            (present pane 'more-type)))))))


(define-presentation-method present (object (type stack-frame) stream
                                            (view minimized-stack-frame-view)
                                            &key acceptably for-context-type)
  (declare (ignore acceptably for-context-type))
  (format t "~A  " (frame-string object))
  (unless (frame-variables object)
    (with-text-size (stream :small)
      (terpri stream)
      (princ "(no locals)" stream))))

(define-presentation-method present (object (type stack-frame) stream
                                            (view maximized-stack-frame-view)
                                            &key acceptably for-context-type)
  (declare (ignore acceptably for-context-type))
  (progn
    (filling-output (stream :fill-width 400)
                    (princ (frame-string object) stream))
    (fresh-line)
    (cond 
      ((frame-variables object)
       (with-text-family (stream :sans-serif)
         (with-text-face (stream :bold)
           (format t "  Locals:")))
       (fresh-line)
       (format t "     ")
       (formatting-table (stream)
                         (loop for (name n identifier id value val) in (frame-variables object)
                            do (formatting-row (stream)
                                               (formatting-cell (stream) (format t "~A" n))
                                               (formatting-cell (stream) (format t "="))
                                               (formatting-cell (stream) (present val 'inspect))))))
      (t (with-text-size (stream :small)
           (princ "(no locals)" stream))))
    (fresh-line)))

(define-presentation-method present (object (type restart) stream
                                            (view textual-view)
                                            &key acceptably for-context-type)
  (declare (ignore acceptably for-context-type))
  (with-text-face (stream :bold)
    (substitute #\Space #\- 
                (string-capitalize (symbol-name (restart-name object))))))

(define-presentation-method present (object (type more-type) stream
                                            (view textual-view)
                                            &key acceptably for-context-type)
  (declare (ignore acceptably for-context-type))
  (with-text-face (stream :bold)
    (with-text-size (stream :large)
      (format t " Show 10 more… "))))

(define-presentation-method present (object (type inspect) stream
                                            (view textual-view)
                                            &key acceptably for-context-type)
  (declare (ignore acceptably for-context-type))
  (format t "~A" object))


;;;
;;;   Starting the debugger 
;;;

(defun debugger-restart (restart me-or-my-encapsulation)
  (if restart
      (let ((*debugger-hook* me-or-my-encapsulation))
        (invoke-restart-interactively restart))
      (abort)))

(defun debugger (condition me-or-my-encapsulation)
  (swank-backend::call-with-debugging-environment 
   (lambda ()
     (unwind-protect
          (run-debugger-frame 
           (make-instance 
            'condition-info
            :the-condition condition
            :type-of-condition (type-of condition)
            :condition-message (swank::safe-condition-message condition)
            :condition-extra (swank::condition-extras condition)
            :restarts (compute-restarts)
            :backtrace (compute-backtrace 0 20))
           me-or-my-encapsulation)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;   For testing   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun simple-break ()
  (with-simple-restart  (continue "Continue from interrupt.")
    (let ((*debugger-hook* #'debugger))
      (invoke-debugger 
       (make-condition 'simple-error 
                       :format-control "Debugger test")))))

