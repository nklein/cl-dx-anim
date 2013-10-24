
(in-package :cl-dx-anim)

(defun read-comment-body (&optional (*standard-input* *standard-input*))
  (loop :for in = (read-char)
        :until (or (char= in #\Newline) (char= in #\Return))
        :collecting in))

(defparser comment (seq "//" comment-body
                        #+not (dbg "COMMENT: ~A~%" comment-body))
  comment-body)

(defparser whitespace-char (seq char)
  (assert (member char '(#\Space #\Newline #\Return #\Tab #\Page)))
  char)

(defparser non-whitespace-non-quote (seq char)
  (assert (not (member char '(#\Space #\Newline #\Return #\Tab #\Page
                              #\"))))
  char)

(defparser whitespace-hunk (seq (ch (optional whitespace-char))
                                (cm (optional comment)))
  (unless (or ch cm)
    (error 'empty-whitespace-hunk)))

(defparser whitespace (seq (zero-or-more whitespace-hunk))
  :whitespace)

(defparser space-number (seq whitespace number)
  number)

(defparser space-float (seq whitespace float)
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

(defun read-template-body (&optional (*standard-input* *standard-input*))
  (loop :for in = (read-char)
        :collecting in
        :until (char= in #\})))

(defparser name (one-or-more (token non-whitespace-non-quote))
  (intern (coerce token 'string)))

(defparser template (seq whitespace "template"
                         whitespace name
                         whitespace template-body)
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
                       whitespace ";")
  (make-array '(4 4)
              :element-type 'single-float
              :initial-contents matrix))

(defparser frame-transform (seq whitespace
                                "FrameTransformMatrix"
                                whitespace "{"
                                matrix
                                whitespace "}")
  matrix)

(defparser vertex (seq whitespace (x float) ";"
                       whitespace (y float) ";"
                       whitespace (z float) ";")
  (make-dx-vertex x y z))

(defparser vertex-array (seq whitespace (nverts number) ";"
                             (verts (array-of vertex nverts ","))
                             whitespace ";")
  (make-array nverts
              :element-type 'dx-vertex
              :initial-contents verts))

(defparser face (seq whitespace
                     (nvert-indexes number) ";"
                     (vert-array (array-of number nvert-indexes ";")) ";")
  (make-array nvert-indexes
              :element-type '(and fixnum (integer 0 *))
              :initial-contents vert-array))

(defparser face-array (seq whitespace (nfaces number) ";"
                           (faces (array-of face nfaces ","))
                           whitespace ";")
  (make-array nfaces
              :element-type 'index-array
              :initial-contents faces))

(defparser normal-array (seq whitespace (nnormals number) ";"
                             (normals (array-of vertex nnormals ","))
                             whitespace ";")
  (make-array nnormals
              :element-type 'dx-vertex
              :initial-contents normals))

(defparser mesh-normals (seq whitespace "MeshNormals"
                             whitespace "{"
                             normal-array
                             face-array
                             whitespace "}")
  (list normal-array face-array))

(defparser mesh-material-list (seq whitespace
                                   "MeshMaterialList"
                                   mesh-material-body)
  'xxx)

(defparser mesh-texture-coords (seq whitespace
                                     "MeshTextureCoords"
                                     mesh-material-body)
  'xxx)

(defparser x-skin-mesh-header (seq whitespace "XSkinMeshHeader"
                                   whitespace "{"
                                   whitespace
                                   (max-weights-per-vertex number) ";"
                                   whitespace
                                   (max-weights-per-face number) ";"
                                   whitespace
                                   (nbones number) ";"
                                   whitespace "}")
  (list max-weights-per-vertex max-weights-per-face nbones))


(defparser skin-weights (seq whitespace "SkinWeights"
                             whitespace "{"
                             whitespace "\"" name "\";"
                             whitespace (nweights number) ";"
                             (vertex-indexes (array-of space-number
                                               nweights ",")) ";"
                             (vertex-weights (array-of space-float
                                               nweights ",")) ";"
                             matrix
                             whitespace "}")
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

(defparser mesh (seq whitespace "Mesh"
                     whitespace "{"
                     vertex-array
                     face-array
                     (mesh-normals (optional mesh-normals))
                     (mesh-material-list (optional mesh-material-list))
                     (mesh-texture-coords (optional mesh-texture-coords))
                     (skin-info (optional skin-info))
                     whitespace "}")
  (make-dx-mesh vertex-array
                face-array
                (first mesh-normals)
                (second mesh-normals)
                (and nil 'xxx (first mesh-material-list))
                (and nil 'xxx (second mesh-material-list))))

(defparser frame-body (seq frame-transform
                           (subframes (zero-or-more frame))
                           (meshes (zero-or-more mesh)))
  (list :transform frame-transform
        :subframes subframes
        :meshes meshes))

(defparser frame (seq whitespace "Frame"
                      whitespace name
                      whitespace "{"
                      frame-body
                      whitespace
                      "}")
  (apply #'make-dx-frame :name name frame-body))

(defparser anim-rate (seq whitespace "AnimTicksPerSecond"
                          whitespace "{"
                          whitespace number ";"
                          whitespace "}")
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

(defparser anim-set (seq whitespace "AnimationSet"
                         whitespace anim-set-body)
  (make-dx-anim-set :tbd nil))

(defparser dx (seq "xof " version "txt " float-size
                   (template-list (zero-or-more template)) frame
                   (anim-rate (optional anim-rate 30)) anim-set)
  (make-dx :version version
           :float-size float-size
           :frame frame
           :anim-rate anim-rate
           :anim-set anim-set))
