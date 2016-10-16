(in-package :lode)

(defun create-joint-group ()
  (clear-joint-group
   (make-phys-joint-group :ptr (djointgroupcreate 0))))

(defun clear-joint-group (joint-group)
  (let ((ptr (phys-joint-group-ptr joint-group)))
    (djointgroupempty ptr)
    joint-group))
