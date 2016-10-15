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

(defun %default-near-callback-func (d g0 g1)
  (declare (cffi:foreign-pointer d g0 g1)
           (ignore d g0 g1))
  (values))

(declaim
 (type (function (cffi:foreign-pointer cffi:foreign-pointer cffi:foreign-pointer)
                 (values))
       *lisp-near-callback-func*))
(defparameter *lisp-near-callback-func* #'%default-near-callback-func)

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

(defconstant +step-col-skip-size+ (cffi:foreign-type-size 'dcontact))

(cffi:defcstruct col-data
  (world-ptr :pointer)
  (joint-grp-ptr :pointer))

;;dContactGeom

(cffi:defcallback %step-collisions-callback
    :void ((data :pointer) (geom-id-0 dgeomid) (geom-id-1 dgeomid))
  (let* ((body-0 (dgeomgetbody geom-id-0))
         (body-1 (dgeomgetbody geom-id-1))
         (max-collisions 10))
    (cffi:with-foreign-slots ((world-ptr joint-grp-ptr) data (:struct col-data))
      (cffi:with-foreign-object (contact-arr '(:struct dcontact) max-collisions)
        (let ((col-count
               (dcollide geom-id-0 geom-id-1 max-collisions
                         (cffi:foreign-slot-pointer
                          contact-arr '(:struct dcontact) 'geom)
                         +step-col-skip-size+)))
          (loop :for i :below col-count :do
             (let* ((elem-ptr (cffi:mem-aptr contact-arr
                                             '(:struct dcontact)
                                             i))
                    (surface (cffi:foreign-slot-pointer
                              elem-ptr
                              '(:struct dcontact)
                              'surface)))
               (cffi:with-foreign-slots
                   ((mode mu mu2 bounce bounce-vel soft-cfm)
                    surface (:struct dsurfaceparameters))
                 (setf mode (logior dcontactbounce dcontactsoftcfm))
                 (setf mu most-positive-single-float)
                 (setf mu2 0s0)
                 (setf bounce 0.01)
                 (setf bounce-vel 0.1)
                 (setf soft-cfm 0.01))
               (djointattach
                (djointcreatecontact world-ptr joint-grp-ptr elem-ptr)
                body-0 body-1)))))))
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

(defun connected-by-a-point-p (body-0 body-1)
  (and (not (cffi:null-pointer-p body-0))
       (not (cffi:null-pointer-p body-1))
       (dareconnectedexcluding
        body-0 body-1 djointtypecontact)))
