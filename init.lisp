(in-package :lode)

(defvar *initialized* nil)

(defun %init ()
  (if *initialized*
      (progn
        (warn "Lode: Will not initalize ODE as it is already initialized")
        nil)
      (progn
        (dinitode)
        (setf *initialized* t))))

(defun %uninit ()
  (when *initialized*
    (dcloseode)
    (setf *initialized* nil)
    t))

(defun init ()
  (when (%init)
    (reset-worlds)
    t))

(defun uninit ()
  (when (%uninit)
    (reset-worlds)))

(defmacro with-ode-initialized (&body body)
  `(unwind-protect (progn (init) ,@body)
     (uninit)))
