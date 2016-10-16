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
  ;;
  (unless my-world ;;fex possible ordering issues? joint could be made before soem of these
    (setf my-world (create-world))
    (setf (world-gravity my-world) (v! 0 -1 0))
    (setf (world-constraint-force-mixing my-world) 1e-5)
    (setf (world-auto-disable my-world) t))
    (setf (world-linear-damping my-world) 0.00001);;fex
    (setf (world-angular-damping my-world) 0.005) ;;fex
    (setf (world-max-angular-speed my-world) 200);;fex
    (setf (world-contact-max-correcting-velocity my-world) 0.1);;fex
    (setf (world-contact-surface-layer my-world) 0.001);;fex

  (dWorldSetAutoDisableAverageSamplesCount (phys-world-ptr my-world) 10) ;;fex

  ;;
  (unless my-col-space
    (setf my-col-space (create-collision-space :kind :hash)))
  ;;
  (unless my-joint-group
    (setf my-joint-group (create-joint-group)))
  ;;
  (unless my-plane
    (setf my-plane (create-plane my-col-space (v! 0 1 0) 0s0)))

  ;;
  (unless my-obj
    (let ((radius 0.3))
      (setf my-obj (make-phys-object my-world))
      (setf (phys-object-position my-obj) (v! 2 20 -5))
      (setf (phys-object-linear-velocity my-obj) (v! 0 0 0))
      (phys-object-set-mass-sphere my-obj 5 radius) ;;fex
      (phys-object-add-geometry my-obj (create-sphere my-col-space radius))))
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
  (step-world my-world 0.02)
  (clear-joint-group my-joint-group))

(defun step-loop ()
  (step-physics)
  (print "--")
  (print (phys-object-position my-obj))
  (print (phys-geometry-position (aref (phys-object-geometries my-obj) 0))))
