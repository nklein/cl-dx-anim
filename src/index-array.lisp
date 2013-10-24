
(in-package :cl-dx-anim)

(deftype index-array (&optional (max '*))
  `(array (array (and fixnum (integer 0 ,max)) (*)) (*)))
