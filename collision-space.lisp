(in-package :lode)

(defun create-collision-space (&key (kind :simple) parent-space)
  (let* ((parent-id (if parent-space
                        (phys-collision-space-ptr parent-space)
                        (cffi:null-pointer)))
         (space-ptr
          (case kind
            (:simple (dsimplespacecreate parent-id))
            (otherwise (error "Collision space kind ~s not known" kind)))))
    (make-phys-collision-space :ptr space-ptr)))
