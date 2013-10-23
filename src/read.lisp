
(in-package :cl-dx-anim)

(defparser space-number (seq (zero-or-more whitespace) number)
  number)

(defparser space-float (seq (zero-or-more whitespace) float)
  float)

(defparser two-digit-number (seq (a digit) (b digit))
  (+ (* a 10) b))
(defparser four-digit-number (seq (a digit) (b digit) (c digit) (d digit))
  (+ (* (+ (* (+ (* a 10) b) 10) c) 10) d))

(defparser major-version (seq two-digit-number) two-digit-number)
(defparser minor-version (seq two-digit-number) two-digit-number)

(defparser version (seq major-version minor-version)
  (check-type major-version (integer 3 3))
  (check-type minor-version (integer 1 3))
  (make-dx-version major-version minor-version))

(defparser float-size (seq four-digit-number) four-digit-number)

(defun read-comment-body (&optional (*standard-input* *standard-input*))
  (loop :for in = (read-char)
        :until (or (char= in #\Newline) (char= in #\Return))
        :collecting in))

(defparser comment (seq "//" comment-body)
  comment-body)

(defparser whitespace (seq char (optional comment))
  (assert (member char '(#\Space #\Newline #\Return #\Tab #\Page)))
  char)

(defparser non-whitespace-non-quote (seq char)
  (assert (not (member char '(#\Space #\Newline #\Return #\Tab #\Page
                              #\"))))
  char)

(defun read-template-body (&optional (*standard-input* *standard-input*))
  (loop :for in = (read-char)
        :collecting in
        :until (char= in #\})))

(defparser name (one-or-more (token non-whitespace-non-quote))
  (intern (coerce token 'string)))

(defparser template (seq (zero-or-more whitespace) "template"
                         (zero-or-more whitespace) name
                         (zero-or-more whitespace) template-body)
  :template)

(defun read-mesh-material-body (&optional (*standard-input* *standard-input*))
  (loop :with depth = 0
        :for in = (read-char)
        :collecting in
        :when (char= in #\{) :do (incf depth)
        :when (char= in #\}) :do (decf depth)
        :until (and (zerop depth) (char= in #\}))))

(defun read-float (&optional (*standard-input* *standard-input*))
  (let ((digits
         (loop :with  point = nil
               :with digit = nil
               :for first = t :then nil
               :for in = (read-char)
               :while (or (and first (or (char= in #\+) (char= in #\-)))
                          (and (not point) (char= in #\.))
                          (digit-char-p in))
               :collecting in
               :when (char= in #\.) :do (setf point t)
               :when (digit-char-p in) :do (setf digit t)
               :finally (progn
                          (unread-char in)
                          (assert digit)))))
    (with-input-from-string (in (coerce digits 'string))
      (let ((*read-eval* nil))
        (coerce (read in) 'single-float)))))

(defparser matrix-row (seq (row (array-of space-float 4 ",")))
  row)

(defparser matrix (seq (matrix (array-of matrix-row 4 ",")) ";"
                       (zero-or-more whitespace) ";")
  (make-array '(4 4)
              :element-type 'single-float
              :initial-contents matrix))

(defparser frame-transform (seq (zero-or-more whitespace)
                                "FrameTransformMatrix"
                                (one-or-more whitespace) "{"
                                matrix
                                (zero-or-more whitespace) "}")
  matrix)

(defparser vertex (seq (zero-or-more whitespace) (x float) ";"
                       (zero-or-more whitespace) (y float) ";"
                       (zero-or-more whitespace) (z float) ";")
  (make-dx-vertex x y z))

(defparser vertex-array (seq (zero-or-more whitespace) (nverts number) ";"
                             (verts (array-of vertex nverts ","))
                             (zero-or-more whitespace) ";")
  (make-array nverts
              :element-type 'dx-vertex
              :initial-contents verts))

(defparser face (seq (zero-or-more whitespace)
                     (nvert-indexes number) ";"
                     (vert-array (array-of number nvert-indexes ";")) ";")
  (make-array nvert-indexes
              :element-type '(and fixnum (integer 0 *))
              :initial-contents vert-array))

(defparser face-array (seq (zero-or-more whitespace) (nfaces number) ";"
                           (faces (array-of face nfaces ","))
                           (zero-or-more whitespace) ";")
  (make-array nfaces
              :element-type 'index-array
              :initial-contents faces))

(defparser normal-array (seq (zero-or-more whitespace) (nnormals number) ";"
                             (normals (array-of vertex nnormals ","))
                             (zero-or-more whitespace) ";")
  (make-array nnormals
              :element-type 'dx-vertex
              :initial-contents normals))

(defparser mesh-normals (seq (one-or-more whitespace) "MeshNormals"
                             (one-or-more whitespace) "{"
                             normal-array
                             face-array
                             (zero-or-more whitespace) "}")
  (list normal-array face-array))

(defparser mesh-material-list (seq (one-or-more whitespace)
                                   "MeshMaterialList"
                                   mesh-material-body)
  nil)

(defparser mesh-texture-coords (seq (one-or-more whitespace)
                                     "MeshTextureCoords"
                                     mesh-material-body)
  nil)

(defparser x-skin-mesh-header (seq (one-or-more whitespace) "XSkinMeshHeader"
                                   (one-or-more whitespace) "{"
                                   (zero-or-more whitespace)
                                   (max-weights-per-vertex number) ";"
                                   (zero-or-more whitespace)
                                   (max-weights-per-face number) ";"
                                   (zero-or-more whitespace)
                                   (nbones number) ";"
                                   (one-or-more whitespace) "}")
  (list max-weights-per-vertex max-weights-per-face nbones))


(defparser skin-weights (seq (one-or-more whitespace) "SkinWeights"
                             (one-or-more whitespace) "{"
                             (zero-or-more whitespace) "\"" name "\";"
                             (zero-or-more whitespace) (nweights number) ";"
                             (vertex-indexes (array-of space-number
                                               nweights ",")) ";"
                             (vertex-weights (array-of space-float
                                               nweights ",")) ";"
                             matrix
                             (one-or-more whitespace) "}")
  (list name
        (make-array nweights
                    :element-type '(and fixnum (integer 0 *))
                    :initial-contents vertex-indexes)
        (make-array nweights
                    :element-type 'single-float
                    :initial-contents vertex-weights)
        matrix))

(defparser skin-info (seq x-skin-mesh-header
                          (skin-weights (array-of skin-weights
                                          (third x-skin-mesh-header))))
  (list* x-skin-mesh-header skin-weights))

(defparser mesh (seq (zero-or-more whitespace) "Mesh"
                     (one-or-more whitespace) "{"
                     vertex-array
                     face-array
                     (mesh-normals (optional mesh-normals))
                     (mesh-material-list (optional mesh-material-list))
                     (mesh-texture-coords (optional mesh-texture-coords))
                     (skin-info (optional skin-info))
                     (zero-or-more whitespace) "}")
  (make-dx-mesh vertex-array
                face-array
                (first mesh-normals)
                (second mesh-normals)
                (first mesh-material-list)
                (second mesh-material-list)))

(defparser frame-body (seq frame-transform
                        (subframes (zero-or-more frame))
                        (meshes (zero-or-more mesh)))
  (list :transform frame-transform
        :subframes subframes
        :meshes meshes))

(defparser frame (seq (zero-or-more whitespace) "Frame"
                      (one-or-more whitespace) name
                      (one-or-more whitespace) "{"
                      frame-body
                      (zero-or-more whitespace) "}")
  (apply #'make-dx-frame :name name frame-body))

(defparser anim-rate (seq (zero-or-more whitespace) "AnimTicksPerSecond"
                          (zero-or-more whitespace) "{"
                          (zero-or-more whitespace) number ";"
                          (zero-or-more whitespace) "}")
  number)

(defparser optional-anim-rate (optional anim-rate)
  (or anim-rate 30))

(defun read-anim-set-body (&optional (*standard-input* *standard-input*))
  (loop :with depth = 0
        :for in = (read-char)
        :collecting in
        :when (char= in #\{) :do (incf depth)
        :when (char= in #\}) :do (decf depth)
        :until (and (zerop depth) (char= in #\}))))

(defparser anim-set (seq (zero-or-more whitespace) "AnimationSet"
                         (zero-or-more whitespace) anim-set-body)
  (make-dx-anim-set :tbd nil))

(defparser dx (seq "xof " version "txt " float-size
                   (template-list (zero-or-more template)) frame
                   (anim-rate (optional anim-rate 30)) anim-set)
  (make-dx :version version
           :float-size float-size
           :frame frame
           :anim-rate anim-rate
           :anim-set anim-set))
