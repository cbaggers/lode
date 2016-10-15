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

(defun init-play ()
  (init)
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
                                       (- (* (random 1s0) 10) 5)))
    (phys-object-set-mass-box my-obj 0.5 (v! 2 2 2))
    (phys-object-add-geometry my-obj (create-box my-col-space (v! 2 2 2))))
  t)

(defun deinit ()
  ;; {TODO}
  )

;; walk-potentially-colliding determines which pairs of geoms in the
;; space we pass to it may potentially intersect. We must also pass the address
;; of a callback function that we will provide. The callback function is
;; responsible for determining which of the potential intersections
;; are actual collisions before adding the collision joints to our
;; joint group called contactgroup, this gives us the chance to set
;; the behaviour of these joints before adding them to the group. The
;; second parameter is a pointer to any data that we may want to pass
;; to our callback routine.  We will cover the details of the
;; nearCallback routine in the next section.

(defun step-physics ()
  (step-collisions my-world my-col-space my-joint-group)
  (step-world my-world)
  (clear-joint-group my-joint-group))

(defun step-loop ()
  (step-physics)
  (print (phys-object-position my-obj)))
