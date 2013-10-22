
(in-package :cl-dx-anim)

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

(defparser non-whitespace (seq char)
  (assert (not (member char '(#\Space #\Newline #\Return #\Tab #\Page))))
  char)

(defun read-template-body (&optional (*standard-input* *standard-input*))
  (loop :for in = (read-char)
        :collecting in
        :until (char= in #\})))

(defparser name (one-or-more (token non-whitespace))
  (intern (coerce token 'string)))

(defparser template (seq (zero-or-more whitespace) "template"
                         (zero-or-more whitespace) name
                         (zero-or-more whitespace) template-body)
  :template)

(defun read-mesh-body (&optional (*standard-input* *standard-input*))
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

(defparser matrix-row (seq (zero-or-more whitespace) (e1 float) ","
                           (zero-or-more whitespace) (e2 float) ","
                           (zero-or-more whitespace) (e3 float) ","
                           (zero-or-more whitespace) (e4 float))
  (list e1 e2 e3 e4))

(defparser matrix (seq (r1 matrix-row) "," (zero-or-more whitespace)
                       (r2 matrix-row) "," (zero-or-more whitespace)
                       (r3 matrix-row) "," (zero-or-more whitespace)
                       (r4 matrix-row) ";" (zero-or-more whitespace) ";")
  (make-array '(4 4)
              :element-type 'single-float
              :initial-contents (list r1 r2 r3 r4)))

(defparser frame-transform (seq (zero-or-more whitespace)
                                "FrameTransformMatrix"
                                (one-or-more whitespace) "{"
                                matrix
                                (zero-or-more whitespace) "}")
  matrix)

(defparser mesh (seq (zero-or-more whitespace) "Mesh"
                     (one-or-more whitespace) mesh-body)
  :mesh)

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
