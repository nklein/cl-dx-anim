(in-package :cl-dx-anim)

(defstruct dx-frame
  (name nil :type symbol :read-only t)
  (transform nil :type (array single-float (4 4)) :read-only t)
  (subframes nil :type list :read-only t)
  (meshes nil :type list :read-only t))
