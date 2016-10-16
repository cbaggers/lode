(in-package :lode)

;;------------------------------------------------------------

(defun create-plane (parent-space
                     &optional (plane-normal (v! 0 1 0)) (distance 0s0))
  (let* ((space-ptr (phys-collision-space-ptr parent-space))
         (normal (v3:normalize plane-normal)))
    (make-phys-plane
     :ptr (dcreateplane space-ptr (v:x normal) (v:y normal) (v:z normal)
                        (float distance)))))

;;------------------------------------------------------------

(defun create-box (space side-lengths-vec3)
  (make-phys-box
   :ptr (dcreatebox (phys-collision-space-ptr space)
                    (v:x side-lengths-vec3)
                    (v:y side-lengths-vec3)
                    (v:z side-lengths-vec3))))

;;------------------------------------------------------------

(defun create-sphere (space radius)
  (make-phys-sphere
   :ptr (dcreatesphere (phys-collision-space-ptr space)
                       (float radius))))

;; dcreatecapsule
;; dcreateconvex
;; dcreatecylinder
;; dcreategeom
;; dcreategeomclass
;; dcreategeomtransform
;; dcreateheightfield
;; dcreateplane
;; dcreateray
;; dcreatetrimesh
