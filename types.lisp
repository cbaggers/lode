(in-package :lode)

;;------------------------------------------------------------

(deftclass phys-collision-space
  ptr)

;;------------------------------------------------------------

(deftclass (phys-body (:constructor %make-phys-body))
  ptr)

;;------------------------------------------------------------

(deftclass (phys-object (:constructor %make-phys-object))
  (body (error "a body must be provided") :type phys-body)
  (geometries (make-array 0 :element-type 'phys-geometry
                          :fill-pointer 0 :adjustable t)
              :type (array phys-geometry (*))))

;;------------------------------------------------------------

(deftclass phys-geometry
  ptr)

(deftclass (phys-plane (:include phys-geometry)))

;;------------------------------------------------------------

(deftclass phys-joint-group
  ptr)

;;------------------------------------------------------------

(deftclass phys-world
  (ptr (error "ptr is mandatory") :type cffi:foreign-pointer)
  (objects (make-array 0 :adjustable t :fill-pointer 0
                       :element-type 'phys-object)
           :type (array phys-object (*))))
