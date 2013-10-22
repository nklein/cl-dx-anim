(defpackage :cl-dx-anim
  (:use :cl)
  (:export :read-dx)
  (:export :dx-version-major
           :dx-version-minor)
  (:export :dx-version
           :dx-float-size
           :dx-frame
           :dx-anim-rate
           :dx-anim-set)
  (:export :dx-frame-name
           :dx-frame-transform
           :dx-frame-subframes
           :dx-frame-meshes))
