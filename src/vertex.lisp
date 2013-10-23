
(in-package :cl-dx-anim)

(defstruct (dx-vertex (:constructor make-dx-vertex (x y z)))
  (x nil :type single-float :read-only t)
  (y nil :type single-float :read-only t)
  (z nil :type single-float :read-only t))
