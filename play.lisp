(in-package :lode)

;; - the dWorld object is a container for all the rigid bodies and their joints.
;; - the dSpace object is similar to the world container except that it applies
;;   to collision instead of dynamics.
;; - the dJointGroup object is a collection of joints.

(defvar my-world nil)

(defvar my-col-space nil)

(defvar my-joint-group nil)

(defvar my-plane nil)

(defvar my-obj nil)

(defun init ()
  (unless my-world
    (setf my-world (create-world))
    (setf (world-gravity my-world) (v! 0 -1 0))
    (setf (world-contact-max-correcting-velocity my-world) 0.9)
    (setf (world-contact-surface-layer my-world) 0.001)
    (setf (world-auto-disable my-world) t))
  (unless my-col-space
    (setf my-col-space (create-collision-space)))
  (unless my-joint-group
    (setf my-joint-group (create-joint-group)))
  (unless my-plane
    (setf my-plane (create-plane my-col-space)))
  (unless my-obj
    (setf my-obj (make-phys-object my-world))
    (setf (phys-object-position my-obj) (v! 0 10 -5))
    (setf (phys-object-rotation my-obj)
          (m3:rotation-from-axis-angle (v! (- (* (random 1s0) 2s0) 1s0)
                                           (- (* (random 1s0) 2s0) 1s0)
                                           (- (* (random 1s0) 2s0) 1s0))
                                       (- (* (random 1s0) 10) 5)))))
