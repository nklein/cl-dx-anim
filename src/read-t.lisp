(in-package :cl-dx-anim-tests)

(nst:def-fixtures sample-dx ()
  (sample-dx (with-input-from-string (in +sample-dx-file+)
               (read-dx in))))

(nst:def-test-group read-tests (sample-dx)
  (nst:def-test reads-sample (:true)
    sample-dx)

  (nst:def-test correct-version (:values (:equal 3) (:equal 3))
    (let ((version (dx-version sample-dx)))
      (values (dx-version-major version)
              (dx-version-minor version))))

  (nst:def-test correct-float-size (:equal 32)
    (dx-float-size sample-dx)))
