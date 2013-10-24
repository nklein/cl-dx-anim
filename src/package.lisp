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
  (:export :dx-vertex-x
           :dx-vertex-y
           :dx-vertex-z)
  (:export :dx-skin-weights-name
           :dx-skin-weights-vertex-indexes
           :dx-skin-weights-weights)
  (:export :dx-mesh-vertexes
           :dx-mesh-faces
           :dx-mesh-normals
           :dx-mesh-face-normals
           :dx-mesh-materials
           :dx-mesh-face-materials)
  (:export :dx-frame-name
           :dx-frame-transform
           :dx-frame-subframes
           :dx-frame-meshes))
