;;;; lode.asd

#+sbcl
(eval-when (:compile-toplevel :load-toplevel :execute)
  (sb-int:set-floating-point-modes :traps nil))

(asdf:defsystem #:lode
  :description "Lispy abstraction over ODE"
  :author "Baggers <techsnuffle@gmail.com>"
  :license "BSD 2 Clause"
  :encoding :utf-8
  :serial t
  :depends-on (#:raw-bindings-ode
               #:cffi #:uiop #:structy-defclass #:rtg-math
               #:fn)
  :components ((:file "package")
               (:file "vars")
               (:file "extra-bindings")
               (:file "helpers")
               (:file "types")
               (:file "worlds")
               (:file "init")
               (:file "collision-space")
               (:file "joints")
               (:file "geometry")
               (:file "body")
               (:file "object")
               (:file "play")))
