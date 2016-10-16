(in-package :lode)

;;------------------------------------------------------------

(defun create-collision-space (&key (kind :simple) parent-space)
  (let* ((parent-id (if parent-space
                        (phys-collision-space-ptr parent-space)
                        (cffi:null-pointer)))
         (space-ptr
          (case kind
            (:simple (dsimplespacecreate parent-id))
            (:hash (dhashspacecreate parent-id))
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

(defun connected-by-a-point-p (body-0 body-1)
  (and (not (cffi:null-pointer-p body-0))
       (not (cffi:null-pointer-p body-1))
       (dareconnectedexcluding
        body-0 body-1 djointtypecontact)))

(cffi:defcallback %step-collisions-callback
    :void ((data :pointer) (geom-0 dgeomid) (geom-1 dgeomid))
  (let* ((body-0 (dgeomgetbody geom-0))
         (body-1 (dgeomgetbody geom-1))
         (max-contacts 8))
    (unless (connected-by-a-point-p body-0 body-1)
      (cffi:with-foreign-slots ((world-ptr joint-grp-ptr) data (:struct col-data))
        (cffi:with-foreign-object (contact-arr '(:struct dcontact) max-contacts)
          (let ((col-count
                 (dcollide geom-0 geom-1 max-contacts
                           (cffi:foreign-slot-pointer
                            contact-arr '(:struct dcontact) 'geom)
                           +step-col-skip-size+)))
            (loop :for i :below col-count :do
               (let* ((elem-ptr (cffi:mem-aptr
                                 contact-arr '(:struct dcontact) i))
                      (surface (cffi:foreign-slot-pointer
                              elem-ptr '(:struct dcontact) 'surface)))
                 (populate-contact-entry
                  surface
                  :mode (logior dcontactbounce dcontactsoftcfm ;;dcontactapprox1
                                )
                  :mu sb-ext:single-float-positive-infinity
                  :mu2 0s0
                  :bounce 0.1
                  :bounce-vel 0.1
                  :soft-cfm 0.01 ;;fex 1e-5
                  )
                 (let ((c (djointcreatecontact world-ptr joint-grp-ptr elem-ptr)))
                   (djointattach c body-0 body-1)))))))))
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

(defun populate-contact-entry
    (surface-ptr
     &key (mode 0)
       (soft-cfm 0s0) (soft-erp 0s0)
       (mu 0s0) (mu2 0s0)
       (bounce 0s0) (bounce-vel 0s0)
       (rho 0s0) (rho2 0s0) (rhon 0s0)
       (slip1 0s0) (slip2 0s0)
       (motion1 0s0) (motion2 0s0) (motionn 0s0))
  ;;
  (let ((mode^ mode)
        (soft-cfm^ soft-cfm)
        (soft-erp^ soft-erp)
        (mu^ mu)
        (mu2^ mu2)
        (bounce^ bounce)
        (bounce-vel^ bounce-vel)
        (rho^ rho)
        (rho2^ rho2)
        (rhon^ rhon)
        (slip1^ slip1)
        (slip2^ slip2)
        (motion1^ motion1)
        (motion2^ motion2)
        (motionn^ motionn))
    (cffi:with-foreign-slots
        ((mode soft-cfm soft-erp
               mu mu2
               bounce bounce-vel
               rho rho2 rhon
               slip1 slip2
               motion1 motion2 motionn)
         surface-ptr (:struct dsurfaceparameters))
      (setf mode mode^)
      (setf soft-cfm (float soft-cfm^))
      (setf soft-erp (float soft-erp^))
      (setf mu (float mu^))
      (setf mu2 (float mu2^))
      (setf bounce (float bounce^))
      (setf bounce-vel (float bounce-vel^))
      (setf rho (float rho^))
      (setf rho2 (float rho2^))
      (setf rhon (float rhon^))
      (setf slip1 (float slip1^))
      (setf slip2 (float slip2^))
      (setf motion1 (float motion1^))
      (setf motion2 (float motion2^))
      (setf motionn (float motionn^))))
  surface-ptr)
