(defpackage :cl-dx-anim-tests
  (:use :cl :cl-dx-anim)
  (:export :run-tests))

(in-package :cl-dx-anim-tests)

(defun run-tests ()
  (let ((*print-pretty* t))
    (nst:nst-cmd :run-package #.*package*)))

(defun exported-function-p (name &optional (package :cl-dx-anim))
  (multiple-value-bind (sym scope) (find-symbol name package)
    (and sym
         (functionp (symbol-function sym))
         (eq scope :external))))

(nst:def-criterion (:exported-function (package) (name))
  (cond
    ((exported-function-p name package)
     (nst:make-success-report))
    (t
     (nst:make-failure-report :format "~A not exported from ~A"
                              :args (list name package)))))

(nst:def-criterion-alias (:dx-anim-func) '(:exported-function :cl-dx-anim))

(nst:def-test-group package-tests ()
  (nst:def-test exports-read-dx (:dx-anim-func)
    "READ-DX")

  (nst:def-test exports-dx-accessors (:each :dx-anim-func)
    '("DX-VERSION"
      "DX-FLOAT-SIZE"
      "DX-FRAME"
      "DX-ANIM-RATE"
      "DX-ANIM-SET"))

  (nst:def-test exports-dx-version-accessors (:each :dx-anim-func)
    '("DX-VERSION-MAJOR"
      "DX-VERSION-MINOR"))

  (nst:def-test exports-dx-vertex-accessors (:each :dx-anim-func)
    '("DX-VERTEX-X"
      "DX-VERTEX-Y"
      "DX-VERTEX-Z"))

  (nst:def-test exports-dx-mesh-accessors (:each :dx-anim-func)
    '("DX-MESH-VERTEXES"
      "DX-MESH-FACES"
      "DX-MESH-NORMALS"
      "DX-MESH-FACE-NORMALS"
      "DX-MESH-MATERIALS"
      "DX-MESH-FACE-MATERIALS"))

  (nst:def-test exports-dx-frame-accessors (:each :dx-anim-func)
    '("DX-FRAME-NAME"
      "DX-FRAME-TRANSFORM"
      "DX-FRAME-SUBFRAMES"
      "DX-FRAME-MESHES")))
