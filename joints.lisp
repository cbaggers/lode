(in-package :lode)

(deftclass phys-joint-group
  ptr)

(defun create-joint-group ()
  (make-phys-joint-group :ptr (djointgroupcreate 0)))
