
(in-package :cl-dx-anim)

(defstruct (dx-skin-weights (:constructor make-dx-skin-weights
                                          (name vertex-indexes
                                                weights)))
  (name nil :type symbol :read-only t)
  (vertex-indexes nil :type index-array :read-only t)
  (weights nil :type (array single-float (*)) :read-only t))
