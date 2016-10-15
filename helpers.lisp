(in-package :lode)

(defun m3ish (m3)
  (let ((arr (make-array 12 :element-type 'single-float)))
    (setf (aref arr 0) (aref m3 0)
          (aref arr 1) (aref m3 3)
          (aref arr 2) (aref m3 6)

          (aref arr 4) (aref m3 1)
          (aref arr 5) (aref m3 4)
          (aref arr 6) (aref m3 7)

          (aref arr 8) (aref m3 2)
          (aref arr 9) (aref m3 5)
          (aref arr 10) (aref m3 8))
    arr))
