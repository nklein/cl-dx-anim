
(in-package :cl-dx-anim)

(defstruct dx
  (version nil :type dx-version :read-only t)
  (float-size nil :type (or (integer 32 32) (integer 64 64)) :read-only t)
  (frame nil :type dx-frame :read-only t)
  (anim-rate nil :type (satisfies plusp) :read-only t)
  (anim-set nil :type dx-anim-set) :read-only t)
