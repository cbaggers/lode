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

;;------------------------------------------------------------

(defconstant +step-col-skip-size+ (cffi:foreign-type-size 'dgeomid))

(cffi:defcstruct col-data
  (world-ptr :pointer)
  (joint-grp-ptr :pointer))

(cffi:defcallback %step-collisions-callback
    :void ((data :pointer) (geom-id-0 dgeomid) (geom-id-1 dgeomid))
  (let ((max-collisions 10)
        (body-0 (dgeomgetbody geom-id-0))
        (body-1 (dgeomgetbody geom-id-1)))
    (cffi:with-foreign-slots ((world-ptr joint-grp-ptr) data (:struct col-data))
      (cffi:with-foreign-object (contact-geom-arr '(:struct dcontactgeom)
                                                  max-collisions)
        (loop :for i :below (dcollide geom-id-0 geom-id-1 max-collisions
                                      contact-geom-arr +step-col-skip-size+) :do
           (let ((c (djointcreatecontact
                     world-ptr joint-grp-ptr (cffi:mem-aptr contact-geom-arr i))))

             (djointattach c body-0 body-1))))))
  (values))

(defun step-collisions (world collision-space joint-group)
  "Let lode update & handle the collisions"
  (cffi:with-foreign-object (data '(:struct col-data))
    (cffi:with-foreign-slots ((world-ptr joint-grp-ptr) data
                              (:struct col-data))
      (setf world-ptr (phys-world-ptr world))
      (setf joint-grp-ptr (phys-joint-group-ptr joint-group)))
    (dspacecollide (phys-collision-space-ptr collision-space)
                   data
                   (cffi:callback %step-collisions-callback))))
