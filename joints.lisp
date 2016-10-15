(in-package :lode)

(defun create-joint-group ()
  (make-phys-joint-group :ptr (djointgroupcreate 0)))
