
(in-package :cl-dx-anim)

(defstruct (dx-mesh (:constructor make-dx-mesh (vertexes
                                                faces
                                                normals
                                                face-normals
                                                materials
                                                face-materials)))
  (vertexes nil :type (array dx-vertex (*)) :read-only t)
  (faces nil :type index-array :read-only t)
  (normals nil :type (or null (array dx-vertex (*))) :read-only t)
  (face-normals nil :type (or null index-array) :read-only t)
  (materials nil :type (or null (array * (*))) :read-only t)
  (face-materials nil :type (or null index-array) :read-only t))
