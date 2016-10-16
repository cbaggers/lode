(in-package :lode)

;;------------------------------------------------------------

(defun make-phys-body (world)
  (let* ((world-ptr (phys-world-ptr world))
         (bptr (dbodycreate world-ptr)))
    (dbodyenable bptr) ;; from cl-ode
    (%make-phys-body :ptr bptr)))

;;------------------------------------------------------------

(defun phys-body-position (phys-body)
  (let* ((body-ptr (phys-body-ptr phys-body))
         (pos (dbodygetposition body-ptr)))
    (v! (cffi:mem-aref pos :float 0)
        (cffi:mem-aref pos :float 1)
        (cffi:mem-aref pos :float 2))))

(defun (setf phys-body-position) (pos-v3 phys-body)
  (let* ((body-ptr (phys-body-ptr phys-body)))
    (dbodysetposition body-ptr (v:x pos-v3) (v:y pos-v3) (v:z pos-v3))
    pos-v3))

;;------------------------------------------------------------

(defun phys-body-angular-velocity (phys-body)
  (let* ((body-ptr (phys-body-ptr phys-body))
         (pos (dbodygetangularvel body-ptr)))
    (v! (cffi:mem-aref pos :float 0)
        (cffi:mem-aref pos :float 1)
        (cffi:mem-aref pos :float 2))))

(defun (setf phys-body-angular-velocity) (vec3 phys-body)
  (let* ((body-ptr (phys-body-ptr phys-body)))
    (dbodysetangularvel body-ptr (v:x vec3) (v:y vec3) (v:z vec3))
    vec3))

;;------------------------------------------------------------

(defun phys-body-force (phys-body)
  (let* ((body-ptr (phys-body-ptr phys-body))
         (pos (dbodygetforce body-ptr)))
    (v! (cffi:mem-aref pos :float 0)
        (cffi:mem-aref pos :float 1)
        (cffi:mem-aref pos :float 2))))

(defun (setf phys-body-force) (vec3 phys-body)
  (let* ((body-ptr (phys-body-ptr phys-body)))
    (dbodysetforce body-ptr (v:x vec3) (v:y vec3) (v:z vec3))
    vec3))

;;------------------------------------------------------------

(defun phys-body-linear-velocity (phys-body)
  (let* ((body-ptr (phys-body-ptr phys-body))
         (pos (dbodygetlinearvel body-ptr)))
    (v! (cffi:mem-aref pos :float 0)
        (cffi:mem-aref pos :float 1)
        (cffi:mem-aref pos :float 2))))

(defun (setf phys-body-linear-velocity) (vec3 phys-body)
  (let* ((body-ptr (phys-body-ptr phys-body)))
    (dbodysetlinearvel body-ptr (v:x vec3) (v:y vec3) (v:z vec3))
    vec3))

;;------------------------------------------------------------

(defun phys-body-quaternion (phys-body)
  (let* ((body-ptr (phys-body-ptr phys-body))
         (pos (dbodygetquaternion body-ptr)))
    (v! (cffi:mem-aref pos :float 0)
        (cffi:mem-aref pos :float 1)
        (cffi:mem-aref pos :float 2)
        (cffi:mem-aref pos :float 3))))

(defun (setf phys-body-quaternion) (quat phys-body)
  (let* ((body-ptr (phys-body-ptr phys-body)))
    (dbodysetquaternion body-ptr quat)
    quat))

;;------------------------------------------------------------

(defun phys-body-rotation (phys-body)
  (let* ((body-ptr (phys-body-ptr phys-body))
         (pos (dbodygetrotation body-ptr)))
    (m3:make (cffi:mem-aref pos :float 0)
             (cffi:mem-aref pos :float 1)
             (cffi:mem-aref pos :float 2)

             (cffi:mem-aref pos :float 4)
             (cffi:mem-aref pos :float 5)
             (cffi:mem-aref pos :float 6)

             (cffi:mem-aref pos :float 8)
             (cffi:mem-aref pos :float 9)
             (cffi:mem-aref pos :float 10))))

(defun (setf phys-body-rotation) (matrix3 phys-body)
  (let* ((body-ptr (phys-body-ptr phys-body)))
    (dbodysetrotation body-ptr (m3ish matrix3))
    matrix3))

;;------------------------------------------------------------

(defun phys-body-torque (phys-body)
  (let* ((body-ptr (phys-body-ptr phys-body))
         (pos (dbodygettorque body-ptr)))
    (v! (cffi:mem-aref pos :float 0)
        (cffi:mem-aref pos :float 1)
        (cffi:mem-aref pos :float 2))))

(defun (setf phys-body-torque) (vec3 phys-body)
  (let* ((body-ptr (phys-body-ptr phys-body)))
    (dbodysettorque body-ptr (v:x vec3) (v:y vec3) (v:z vec3))
    vec3))

;;------------------------------------------------------------

(defun phys-body-angular-damping (phys-body)
  (let* ((body-ptr (phys-body-ptr phys-body)))
    (dbodygetangulardamping body-ptr)))

(defun (setf phys-body-angular-damping) (value phys-body)
  (let* ((body-ptr (phys-body-ptr phys-body)))
    (dbodysetangulardamping body-ptr (float value))
    value))

;;------------------------------------------------------------

(defun phys-body-angular-damping-threshold (phys-body)
  (let* ((body-ptr (phys-body-ptr phys-body)))
    (dbodygetangulardampingthreshold body-ptr)))

(defun (setf phys-body-angular-damping-threshold) (value phys-body)
  (let* ((body-ptr (phys-body-ptr phys-body)))
    (dbodysetangulardampingthreshold body-ptr (float value))
    value))

;;------------------------------------------------------------

(defun phys-body-auto-disable-angular-threshold (phys-body)
  (let* ((body-ptr (phys-body-ptr phys-body)))
    (dbodygetautodisableangularthreshold body-ptr)))

(defun (setf phys-body-auto-disable-angular-threshold) (value phys-body)
  (let* ((body-ptr (phys-body-ptr phys-body)))
    (dbodysetautodisableangularthreshold body-ptr (float value))
    value))

;;------------------------------------------------------------

(defun phys-body-auto-disable-linear-threshold (phys-body)
  (let* ((body-ptr (phys-body-ptr phys-body)))
    (dbodygetautodisablelinearthreshold body-ptr)))

(defun (setf phys-body-auto-disable-linear-threshold) (value phys-body)
  (let* ((body-ptr (phys-body-ptr phys-body)))
    (dbodysetautodisablelinearthreshold body-ptr (float value))
    value))


;;------------------------------------------------------------

(defun phys-body-auto-disable-time (phys-body)
  (let* ((body-ptr (phys-body-ptr phys-body)))
    (dbodygetautodisabletime body-ptr)))

(defun (setf phys-body-auto-disable-time) (value phys-body)
  (let* ((body-ptr (phys-body-ptr phys-body)))
    (dbodysetautodisabletime body-ptr (float value))
    value))

;;------------------------------------------------------------

(defun phys-body-linear-damping (phys-body)
  (let* ((body-ptr (phys-body-ptr phys-body)))
    (dbodygetlineardamping body-ptr)))

(defun (setf phys-body-linear-damping) (value phys-body)
  (let* ((body-ptr (phys-body-ptr phys-body)))
    (dbodysetlineardamping body-ptr (float value))
    value))

;;------------------------------------------------------------

(defun phys-body-linear-damping-threshold (phys-body)
  (let* ((body-ptr (phys-body-ptr phys-body)))
    (dbodygetlineardampingthreshold body-ptr)))

(defun (setf phys-body-linear-damping-threshold) (value phys-body)
  (let* ((body-ptr (phys-body-ptr phys-body)))
    (dbodysetlineardampingthreshold body-ptr (float value))
    value))

;;------------------------------------------------------------

(defun phys-body-max-angular-speed (phys-body)
  (let* ((body-ptr (phys-body-ptr phys-body)))
    (dbodygetmaxangularspeed body-ptr)))

(defun (setf phys-body-max-angular-speed) (value phys-body)
  (let* ((body-ptr (phys-body-ptr phys-body)))
    (dbodysetmaxangularspeed body-ptr (float value))
    value))

;;------------------------------------------------------------

;; ;; values dvector3
;; dbodygetfiniterotationaxis
;; dbodygetposrelpoint
;; dbodygetrelpointpos
;; dbodygetrelpointvel
;; dbodygetpointvel

;; ;; values dmass
;; dbodygetmass

;; ;;void*
;; dbodygetdata

;; ;;dgeomid
;; dbodygetfirstgeom
;; dbodygetnextgeom

;; ;;int
;; dbodygetautodisableaveragesamplescount
;; dbodygetautodisableflag
;; dbodygetautodisablesteps
;; dbodygetfiniterotationmode
;; dbodygetgravitymode
;; dbodygetgyroscopicmode
;; dbodygetnumjoints

;; ;;djointid
;; dbodygetjoint


;; ;;dworldid
;; dbodygetworld

;;------------------------------------------------------------

(defun phys-body-set-mass-zero (phys-body)
  (let* ((body-ptr (phys-body-ptr phys-body)))
    (cffi:with-foreign-object (m '(:struct dmass))
      (dmasssetzero m)
      (dbodysetmass body-ptr m))
    phys-body))

;;------------------------------------------------------------

(defun phys-body-set-mass-box (phys-body density side-lengths-vec3)
  (let* ((body-ptr (phys-body-ptr phys-body)))
    (cffi:with-foreign-object (m '(:struct dmass))
      (dmasssetbox m (float density)
                   (v:x side-lengths-vec3)
                   (v:y side-lengths-vec3)
                   (v:z side-lengths-vec3))
      (dbodysetmass body-ptr m))
    phys-body))

(defun phys-body-set-mass-box-total (phys-body total-mass side-lengths-vec3)
  (let* ((body-ptr (phys-body-ptr phys-body)))
    (cffi:with-foreign-object (m '(:struct dmass))
      (dmasssetboxtotal m (float total-mass)
                        (v:x side-lengths-vec3)
                        (v:y side-lengths-vec3)
                        (v:z side-lengths-vec3))
      (dbodysetmass body-ptr m))
    phys-body))

;;------------------------------------------------------------

(defun phys-body-set-mass-sphere (phys-body density radius)
  (let* ((body-ptr (phys-body-ptr phys-body)))
    (cffi:with-foreign-object (m '(:struct dmass))
      (dmasssetsphere m (float density) (float radius))
      (dbodysetmass body-ptr m))
    phys-body))

(defun phys-body-set-mass-sphere-total (phys-body total-mass radius)
  (let* ((body-ptr (phys-body-ptr phys-body)))
    (cffi:with-foreign-object (m '(:struct dmass))
      (dmasssetspheretotal m (float total-mass) (float radius))
      (dbodysetmass body-ptr m))
    phys-body))

;;------------------------------------------------------------

(defun phys-body-set-mass-cylinder (phys-body density direction radius length)
  (assert (member direction '(:x :y :z)))
  (let* ((body-ptr (phys-body-ptr phys-body)))
    (cffi:with-foreign-object (m '(:struct dmass))
      (dmasssetcylinder m
                        (float density)
                        (position direction '(:x :y :z))
                        (float radius)
                        (float length))
      (dbodysetmass body-ptr m))
    phys-body))

(defun phys-body-set-mass-cylinder-total (phys-body total-mass direction radius
                                          length)
  (assert (member direction '(:x :y :z)))
  (let* ((body-ptr (phys-body-ptr phys-body)))
    (cffi:with-foreign-object (m '(:struct dmass))
      (dmasssetcylinder m
                        (float total-mass)
                        (position direction '(:x :y :z))
                        (float radius)
                        (float length))
      (dbodysetmass body-ptr m))
    phys-body))

;;------------------------------------------------------------

(defun phys-body-set-mass-capped-cylinder (phys-body density direction radius
                                           length)
  (assert (member direction '(:x :y :z)))
  (let* ((body-ptr (phys-body-ptr phys-body)))
    (cffi:with-foreign-object (m '(:struct dmass))
      (dmasssetcappedcylinder m
                              (float density)
                              (position direction '(:x :y :z))
                              (float radius)
                              (float length))
      (dbodysetmass body-ptr m))
    phys-body))

(defun phys-body-set-mass-capped-cylinder-total (phys-body total-mass direction
                                                 radius length)
  (assert (member direction '(:x :y :z)))
  (let* ((body-ptr (phys-body-ptr phys-body)))
    (cffi:with-foreign-object (m '(:struct dmass))
      (dmasssetcappedcylinder m
                              (float total-mass)
                              (position direction '(:x :y :z))
                              (float radius)
                              (float length))
      (dbodysetmass body-ptr m))
    phys-body))

;;------------------------------------------------------------

(defun phys-body-set-mass-capsule (phys-body density direction radius length)
  (assert (member direction '(:x :y :z)))
  (let* ((body-ptr (phys-body-ptr phys-body)))
    (cffi:with-foreign-object (m '(:struct dmass))
      (dmasssetcapsule m
                        (float density)
                        (position direction '(:x :y :z))
                        (float radius)
                        (float length))
      (dbodysetmass body-ptr m))
    phys-body))

(defun phys-body-set-mass-capsule-total (phys-body total-mass direction radius
                                         length)
  (assert (member direction '(:x :y :z)))
  (let* ((body-ptr (phys-body-ptr phys-body)))
    (cffi:with-foreign-object (m '(:struct dmass))
      (dmasssetcapsule m
                        (float total-mass)
                        (position direction '(:x :y :z))
                        (float radius)
                        (float length))
      (dbodysetmass body-ptr m))
    phys-body))

;;------------------------------------------------------------

;; dmasssettrimesh
