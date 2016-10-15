(in-package :lode)

;;------------------------------------------------------------

(defun make-phys-object (world &optional geometries)
  (let* ((body (make-phys-body world))
         (obj (%make-phys-object :body body)))
    (map nil (lambda (x) (vector-push-extend x (phys-object-geometries obj)))
         geometries)
    (let ((lookup-id (add-phys-object-to-world world obj)))
      (dbodysetdata (phys-body-ptr body) (cffi:make-pointer lookup-id)))
    obj))

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

(defun %phys-body->phys-object (world body)
  (let* ((ptr (dbodygetdata (phys-body-ptr body)))
         (index (cffi:pointer-address ptr)))
    (aref (phys-world-objects world)
          index)))

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
