(in-package :lode)

;;------------------------------------------------------------

(deftclass (phys-object (:constructor %make-phys-object))
  (body (error "a body must be provided") :type phys-body)
  (geometries (make-array 0 :element-type 'phys-geometry
                          :fill-pointer 0 :adjustable t)
              :type (array phys-geometry (*))))

(defun make-phys-object (world &optional geometries)
  (let ((obj (%make-phys-object :body (make-phys-body world))))
    (map nil (lambda (x) (vector-push-extend x (phys-object-geometries obj)))
         geometries)
    obj))

;;------------------------------------------------------------

(defun phys-object-position (phys-object)
  (phys-body-position (phys-object-body phys-object)))

(defun (setf phys-object-position) (pos-v3 phys-object)
  (setf (phys-body-position (phys-object-body phys-object))
        pos-v3))
