(in-package :lode)

;;------------------------------------------------------------

(defun create-collision-space (&key (kind :simple) parent-space)
  (let* ((parent-id (if parent-space
                        (phys-collision-space-ptr parent-space)
                        (cffi:null-pointer)))
         (space-ptr
          (case kind
            (:simple (dsimplespacecreate parent-id))
            (otherwise (error "Collision space kind ~s not known" kind)))))
    (make-phys-collision-space :ptr space-ptr)))

;;------------------------------------------------------------

;; static void nearCallback (void *data, dGeomID o1, dGeomID o2)

(declaim
 (type (function (cffi:foreign-pointer cffi:foreign-pointer cffi:foreign-pointer)
                 (values))
       *lisp-near-callback-func*))
(defparameter *lisp-near-callback-func* #'%default-near-callback-func)

(defun %default-near-callback-func (d g0 g1)
  (declare (cffi:foreign-pointer d g0 g1)
           (ignore d g0 g1))
  (values))

(cffi:defcallback %near-callback
    :void ((data-ptr :pointer) (geom-id-0 dgeomid) (geom-id-1 dgeomid))
  (funcall *lisp-near-callback-func* data-ptr geom-id-0 geom-id-1 )
  nil)

(defun walk-potentially-colliding (collision-space func)
  (declare (type (function (cffi:foreign-pointer
                            cffi:foreign-pointer
                            cffi:foreign-pointer)
                           (values))
                 func))
  (let ((*lisp-near-callback-func* func))
    (dspacecollide (phys-collision-space-ptr collision-space)
                   (cffi:null-pointer)
                   (cffi:callback %near-callback))))
