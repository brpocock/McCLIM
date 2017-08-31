(in-package :mcclim-render-internals)

;;;
;;; aa render functions
;;;

(defmacro def-aa-render-draw-fn (image-class set-fn blend-fn)
  `(progn
     (defmethod aa-render-draw-fn ((image ,image-class) clip-region pixeled-design)
       (let ((set-fn (,set-fn image))
             (blend-fn (,blend-fn image))
             (design-fn (pixeled-rgba-fn pixeled-design)))
         (declare (type ,set-fn set-fn)
                  (type ,blend-fn blend-fn)
                  (type pixeled-design-fn design-fn))
         (lambda (x y alpha)
           (declare (type fixnum x y)
                    (type fixnum alpha))
           (when (or (null clip-region)
                     (clim:region-contains-position-p clip-region x y))
             (setf alpha (min (abs alpha) 255))
             (when (plusp alpha)
               (multiple-value-bind (r.fg g.fg b.fg a.fg)
                   (funcall design-fn x y)
                 (if (> (octet-mult a.fg alpha) 250)
                     (funcall set-fn x y r.fg g.fg b.fg)
                     (funcall blend-fn x y r.fg g.fg b.fg (octet-mult a.fg alpha)))))))))
     (defmethod aa-render-draw-fn ((image ,image-class) clip-region
                                        (pixeled-design pixeled-uniform-design))
       (let ((set-fn (,set-fn image))
             (blend-fn (,blend-fn image)))
         (declare (type ,set-fn set-fn)
                  (type ,blend-fn blend-fn))
         (multiple-value-bind (r.fg g.fg b.fg a.fg)
             (values
              (pixeled-uniform-design-red pixeled-design)
              (pixeled-uniform-design-green pixeled-design)
              (pixeled-uniform-design-blue pixeled-design)
              (pixeled-uniform-design-alpha pixeled-design))
           (declare (type octet r.fg g.fg b.fg a.fg))
           (lambda (x y alpha)
             (declare (type fixnum x y)
                      (type fixnum alpha))
             (when (or (null clip-region)
                       (clim:region-contains-position-p clip-region x y))
               (setf alpha (min (abs alpha) 255))
               (when (plusp alpha)
                 (if (> (octet-mult a.fg alpha) 250)
                     (funcall set-fn x y r.fg g.fg b.fg)
                     (funcall blend-fn x y r.fg g.fg b.fg (octet-mult a.fg alpha)))))))))
     (defmethod aa-render-draw-span-fn ((image ,image-class) clip-region pixeled-design)
       (let ((set-fn (,set-fn image))
             (blend-fn (,blend-fn image))
             (design-fn (pixeled-rgba-fn pixeled-design)))
         (declare (type ,set-fn set-fn)
                  (type ,blend-fn blend-fn)
                  (type pixeled-design-fn design-fn))
         (lambda (x1 x2 y alpha)
           (declare (type fixnum x1 x2 y)
                    (type fixnum alpha))
           (loop for x from x1 below x2 do
                (when (or (null clip-region)
                          (clim:region-contains-position-p clip-region x y))
                  (setf alpha (min (abs alpha) 255))
                  (when (plusp alpha)
                    (multiple-value-bind (r.fg g.fg b.fg a.fg)
                        (funcall design-fn x y)
                      (if (> (octet-mult a.fg alpha) 250)
                          (funcall set-fn x y r.fg g.fg b.fg)
                          (funcall blend-fn x y r.fg g.fg b.fg (octet-mult a.fg alpha))))))))))
     (defmethod aa-render-draw-span-fn ((image ,image-class) clip-region
                                             (pixeled-design pixeled-uniform-design))
       (let ((set-fn (,set-fn image))
             (blend-fn (,blend-fn image)))
         (declare (type ,set-fn set-fn)
                  (type ,blend-fn blend-fn))
         (multiple-value-bind (r.fg g.fg b.fg a.fg)
             (values
              (pixeled-uniform-design-red pixeled-design)
              (pixeled-uniform-design-green pixeled-design)
              (pixeled-uniform-design-blue pixeled-design)
              (pixeled-uniform-design-alpha pixeled-design))
           (declare (type octet r.fg g.fg b.fg a.fg))
           (lambda (x1 x2 y alpha)
             (declare (type fixnum x1 x2 y)
                      (type fixnum alpha))
             (loop for x from x1 below x2 do
                  (when (or (null clip-region)
                            (clim:region-contains-position-p clip-region x y))
                    (setf alpha (min (abs alpha) 255))
                    (when (plusp alpha)
                      (if (> (octet-mult a.fg alpha) 250)
                          (funcall set-fn x y r.fg g.fg b.fg)
                          (funcall blend-fn x y r.fg g.fg b.fg (octet-mult a.fg alpha))))))))))))

(defmacro def-aa-render-alpha-draw-fn (image-class)
  `(progn
     (defmethod aa-render-alpha-draw-fn ((image ,image-class) clip-region)
       (let ((set-fn (image-gray-set-fn image)))
         (declare (type image-gray-set-fn set-fn))
         (lambda (x y alpha)
           (declare (type fixnum x y)
                    (type fixnum alpha))
           (when (or (null clip-region)
                     (clim:region-contains-position-p clip-region x y))
             (setf alpha (min (abs alpha) 255))
             (when (plusp alpha)
               (funcall set-fn x y alpha))))))
     (defmethod aa-render-alpha-draw-span-fn ((image ,image-class) clip-region)
       (let ((set-fn (image-gray-set-fn image)))
         (declare (type image-gray-set-fn set-fn))
         (lambda (x1 x2 y alpha)
           (declare (type fixnum x1 x2 y)
                    (type fixnum alpha))
           (loop for x from x1 below x2 do
                (when (or (null clip-region)
                          (clim:region-contains-position-p clip-region x y))
                  (setf alpha (min (abs alpha) 255))
                  (when (plusp alpha)
                    (funcall set-fn x y alpha)))))))))

(defmacro def-aa-render-xor-draw-fn (image-class set-fn blend-fn)
  `(progn
     (defmethod aa-render-xor-draw-fn ((image ,image-class) clip-region pixeled-design)
       (let ((set-fn (,set-fn image))
             (blend-fn (,blend-fn image))
             (design-fn (pixeled-rgba-fn pixeled-design)))
         (declare (type ,set-fn set-fn)
                  (type ,blend-fn blend-fn)
                  (type pixeled-design-fn design-fn)
                  (ignorable set-fn))
         (lambda (x y alpha)
           (declare (type fixnum x y)
                    (type fixnum alpha))
           (when (or (null clip-region)
                     (clim:region-contains-position-p clip-region x y))
             (setf alpha (min (abs alpha) 255))
             (when (plusp alpha)
               (multiple-value-bind (r.fg g.fg b.fg a.fg)
                   (funcall design-fn x y)
                 (funcall blend-fn x y r.fg g.fg b.fg (octet-mult a.fg alpha))))))))
     (defmethod aa-render-xor-draw-span-fn ((image ,image-class) clip-region pixeled-design)
       (let ((set-fn (,set-fn image))
             (blend-fn (,blend-fn image))
             (design-fn (pixeled-rgba-fn pixeled-design)))
         (declare (type ,set-fn set-fn)
                  (type ,blend-fn blend-fn)
                  (type pixeled-design-fn design-fn)
                  (ignorable set-fn))
         (lambda (x1 x2 y alpha)
           (declare (type fixnum x1 x2 y)
                    (type fixnum alpha))
           (loop for x from x1 below x2 do
                (when (or (null clip-region)
                          (clim:region-contains-position-p clip-region x y))
                  (setf alpha (min (abs alpha) 255))
                  (when (plusp alpha)
                    (multiple-value-bind (r.fg g.fg b.fg a.fg)
                        (funcall design-fn x y)
                      (funcall blend-fn x y r.fg g.fg b.fg (octet-mult a.fg alpha)))))))))))

(def-aa-render-draw-fn rgb-image-mixin image-rgb-set-fn image-rgb-blend-fn)
(def-aa-render-xor-draw-fn rgb-image-mixin image-rgb-set-fn image-rgb-xor-blend-fn)
(def-aa-render-alpha-draw-fn gray-image-mixin)
