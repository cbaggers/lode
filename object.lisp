(in-package :lode)
(named-readtables:in-readtable :fn.reader)

;;------------------------------------------------------------

(defun make-phys-object (world &optional geometries)
  (let* ((body (make-phys-body world))
         (obj (%make-phys-object :body body)))
    (map nil λ(phys-object-add-geometry obj _) geometries)
    (let ((lookup-id (add-phys-object-to-world world obj)))
      (dbodysetdata (phys-body-ptr body) (cffi:make-pointer lookup-id)))
    obj))

;;------------------------------------------------------------

(defun phys-object-add-geometry (phys-object geometry)
  (dgeomsetbody (phys-geometry-ptr geometry)
                (phys-body-ptr (phys-object-body phys-object)))
  (vector-push-extend geometry (phys-object-geometries phys-object))
  phys-object)

;;------------------------------------------------------------

(defun %phys-body->phys-object (world body)
  (let* ((ptr (dbodygetdata (phys-body-ptr body)))
         (index (cffi:pointer-address ptr)))
    (aref (phys-world-objects world)
          index)))

;;------------------------------------------------------------

(defmacro gen-body-accessor (name)
  (let ((oname (intern (format nil "PHYS-OBJECT-~a" name)))
        (bname (intern (format nil "PHYS-BODY-~A" name))))
    `(progn
       (defun ,oname (phys-object)
         (,bname (phys-object-body phys-object)))
       (defun (setf ,oname) (value phys-object)
         (setf (,bname (phys-object-body phys-object)) value)))))

;;------------------------------------------------------------

(gen-body-accessor position)
(gen-body-accessor angular-velocity)
(gen-body-accessor force)
(gen-body-accessor linear-velocity)
(gen-body-accessor quaternion)
(gen-body-accessor rotation)
(gen-body-accessor torque)
(gen-body-accessor angular-damping)
(gen-body-accessor angular-damping-threshold)
(gen-body-accessor auto-disable-angular-threshold)
(gen-body-accessor auto-disable-linear-threshold)
(gen-body-accessor auto-disable-time)
(gen-body-accessor linear-damping)
(gen-body-accessor linear-damping-threshold)
(gen-body-accessor max-angular-speed)

;;------------------------------------------------------------

(defun phys-object-set-mass-zero (phys-obj)
  (phys-body-set-mass-zero (phys-object-body phys-obj)))

(defun phys-object-set-mass-box (phys-obj density side-lengths-vec3)
  (phys-body-set-mass-box (phys-object-body phys-obj) density side-lengths-vec3))

(defun phys-object-set-mass-box-total (phys-obj total-mass side-lengths-vec3)
  (phys-body-set-mass-box-total (phys-object-body phys-obj) total-mass side-lengths-vec3))

(defun phys-object-set-mass-sphere (phys-obj density radius)
  (phys-body-set-mass-sphere (phys-object-body phys-obj) density radius))

(defun phys-object-set-mass-sphere-total (phys-obj total-mass radius)
  (phys-body-set-mass-sphere-total (phys-object-body phys-obj) total-mass radius))

(defun phys-object-set-mass-cylinder (phys-obj density direction radius length)
  (phys-body-set-mass-cylinder (phys-object-body phys-obj) density direction radius length))

(defun phys-object-set-mass-cylinder-total (phys-obj total-mass direction radius length)
  (phys-body-set-mass-cylinder-total (phys-object-body phys-obj) total-mass direction radius length))

(defun phys-object-set-mass-capped-cylinder (phys-obj density direction radius length)
  (phys-body-set-mass-capped-cylinder (phys-object-body phys-obj) density direction radius length))

(defun phys-object-set-mass-capped-cylinder-total (phys-obj total-mass direction radius length)
  (phys-body-set-mass-capped-cylinder-total (phys-object-body phys-obj) total-mass direction radius length))

(defun phys-object-set-mass-capsule (phys-obj density direction radius length)
  (phys-body-set-mass-capsule (phys-object-body phys-obj) density direction radius length))

(defun phys-object-set-mass-capsule-total (phys-obj total-mass direction radius length)
  (phys-body-set-mass-capsule-total (phys-object-body phys-obj) total-mass direction radius length))

;;------------------------------------------------------------
