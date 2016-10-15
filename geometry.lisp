(in-package :lode)

;;------------------------------------------------------------

(defun create-plane (parent-space &optional (plane-normal (v! 0 1 0)))
  (let* ((space-ptr (phys-collision-space-ptr parent-space))
         (normal (v3:normalize plane-normal)))
    (make-phys-plane
     :ptr (dcreateplane space-ptr (v:x normal) (v:y normal) (v:z normal) 0s0))))

;;------------------------------------------------------------

(defun create-box (space side-lengths-vec3)
  (make-phys-box
   :ptr (dcreatebox (phys-collision-space-ptr space)
                    (v:x side-lengths-vec3)
                    (v:y side-lengths-vec3)
                    (v:z side-lengths-vec3))))

;;------------------------------------------------------------

;; dcreatecapsule
;; dcreateconvex
;; dcreatecylinder
;; dcreategeom
;; dcreategeomclass
;; dcreategeomtransform
;; dcreateheightfield
;; dcreateplane
;; dcreateray
;; dcreatesphere
;; dcreatetrimesh
