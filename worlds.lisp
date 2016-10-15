(in-package :lode)

(defvar *worlds* (make-array 0 :element-type 'phys-world :adjustable t
                               :fill-pointer 0))

(defun reset-worlds ()
  (setf *worlds* (make-array 0 :element-type 'phys-world :adjustable t
                               :fill-pointer 0)))

(defun create-world ()
  (assert *initialized*)
  (let ((world (make-phys-world :ptr (dworldcreate))))
    (vector-push-extend world *worlds*)
    world))

;;------------------------------------------------------------

(defun add-phys-object-to-world (world phys-obj)
  (vector-push-extend phys-obj (phys-world-objects world)))

;;------------------------------------------------------------

(defun world-gravity (world)
  (let ((ptr (phys-world-ptr world)))
    (cffi:with-foreign-object (g :float 4)
      (dworldgetgravity2 ptr g)
      (v! (cffi:mem-aref g :float 0)
          (cffi:mem-aref g :float 1)
          (cffi:mem-aref g :float 2)))))

(defun (setf world-gravity) (gravity-v3 world)
  (let ((ptr (phys-world-ptr world)))
    (dworldsetgravity
     ptr (v:x gravity-v3) (v:y gravity-v3) (v:z gravity-v3) )
    gravity-v3))

;;------------------------------------------------------------

(defun world-error-reduction-parameter (world)
  (let ((ptr (phys-world-ptr world)))
    (dworldgeterp ptr)))

(defun (setf world-error-reduction-parameter) (value world)
  (let ((ptr (phys-world-ptr world)))
    (dworldseterp ptr (float value))
    value))

(defun world-constraint-force-mixing (world)
  (let ((ptr (phys-world-ptr world)))
    (dworldgetcfm ptr)))

(defun (setf world-constraint-force-mixing) (value world)
  (let ((ptr (phys-world-ptr world)))
    (dworldsetcfm ptr (float value))
    value))

;;------------------------------------------------------------

(defun world-contact-max-correcting-velocity (world)
  "This function sets the velocity that interpenetrating objects will separate
   at. The default value is infinity."
  (let ((ptr (phys-world-ptr world)))
    (dworldgetcontactmaxcorrectingvel ptr)))

(defun (setf world-contact-max-correcting-velocity) (value world)
  (let ((ptr (phys-world-ptr world)))
    (dworldsetcontactmaxcorrectingvel ptr (float value))
    value))

;;------------------------------------------------------------

(defun world-contact-surface-layer (world)
  "This function sets the depth of the surface layer around the world objects.
   Contacts are allowed to sink into each other up to this depth. Setting it to
   a small value reduces the amount of jittering between contacting objects, the
   default value is 0."
  (let ((ptr (phys-world-ptr world)))
    (dworldgetcontactsurfacelayer ptr)))

(defun (setf world-contact-surface-layer) (value world)
  (let ((ptr (phys-world-ptr world)))
    (dworldsetcontactsurfacelayer ptr (float value))
    value))

;;------------------------------------------------------------

(defun world-auto-disable (world)
  "When true this means that objects that have come to rest (based on their
   current linear and angular velocity) will no longer participate in the
   simulation, unless acted upon by a moving object.
   If you do not want to use this feature then set this to nil.
   You can also manually enable or disable objects using #'enable-body and
   #'disable-body."
  (let ((ptr (phys-world-ptr world)))
    (> (dworldgetautodisableflag ptr) 0)))

(defun (setf world-auto-disable) (value world)
  (let ((ptr (phys-world-ptr world)))
    (dworldsetautodisableflag ptr (if value 1 0))
    value))

;;------------------------------------------------------------

(defun step-world (world)
  (dworldquickstep (phys-world-ptr world) 0.04))
