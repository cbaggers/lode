(in-package :lode)

;; needed because the cffi/c2ffi did a weird things. The second arg
;; should be an array to be populated but it treated it as a literal
;; and so expected a lisp array. That cant be mutated by C so we end
;; up with an invalid result
(cffi:defcfun ("dWorldGetGravity" dworldgetgravity2)
    :void
  (arg1 dworldid)
  (gravity :pointer))
