;;;; lode.asd

(asdf:defsystem #:lode
  :description "Lispy abstraction over ODE"
  :author "Baggers <techsnuffle@gmail.com>"
  :license "BSD 2 Clause"
  :serial t
  :depends-on (#:raw-bindings-ode #:cffi #:uiop #:structy-defclass
                                  #:rtg-math #:fn)
  :components ((:file "package")
               (:file "extra-bindings")
               (:file "helpers")
               (:file "types")
               (:file "worlds")
               (:file "init")
               (:file "collision-space")
               (:file "joints")
               (:file "geometry")
               (:file "body")
               (:file "object")))
