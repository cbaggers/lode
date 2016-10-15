(in-package :lode)

;;------------------------------------------------------------

(deftclass phys-geometry
  ptr)

;;------------------------------------------------------------

(deftclass (phys-plane (:include phys-geometry)))

(defun create-plane (parent-space &optional (plane-normal (v! 0 1 0)))
  (let* ((space-ptr (phys-collision-space-ptr parent-space))
         (normal (v3:normalize plane-normal)))
    (make-phys-plane
     :ptr (dcreateplane space-ptr (v:x normal) (v:y normal) (v:z normal) 0s0))))
