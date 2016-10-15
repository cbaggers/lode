(in-package :lode)

;;------------------------------------------------------------

(defun make-phys-body (world)
  (let ((world-ptr (phys-world-ptr world)))
    (%make-phys-body :ptr (dbodycreate world-ptr))))

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
