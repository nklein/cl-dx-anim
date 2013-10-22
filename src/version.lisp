
(in-package :cl-dx-anim)

(defstruct (dx-version (:constructor make-dx-version (major minor)))
  (major nil :type (integer 3 3) :read-only t)
  (minor nil :type (integer 1 3) :read-only t))
