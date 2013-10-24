
(asdf:defsystem :cl-dx-anim
  :description "Library to read DirectX animation files output from Blender"
  :author "Patrick Stein <pat@nklein.com>"
  :version "0.1.20131022"
  :license "UNLICENSE"
  :components
  ((:static-file "README.md")
   (:static-file "UNLICENSE.txt")
   (:module "src"
    :components ((:file "package")
                 (:file "version" :depends-on ("package"))
                 (:file "index-array" :depends-on ("package"))
                 (:file "vertex" :depends-on ("package"))
                 (:file "skin-weights" :depends-on ("package"
                                                    "index-array"))
                 (:file "mesh" :depends-on ("package"
                                            "index-array"
                                            "vertex"
                                            "skin-weights"))
                 (:file "frame" :depends-on ("package"
                                             "mesh"))
                 (:file "anim-set" :depends-on ("package"))
                 (:file "dx" :depends-on ("package"
                                          "version"
                                          "frame"
                                          "anim-set"))
                 (:file "parser-support" :depends-on ("package"))
                 (:file "parser" :depends-on ("package" "parser-support"))
                 (:file "read" :depends-on ("package"
                                            "parser"
                                            "dx"))))))

(asdf:defsystem :cl-dx-anim-tests
  :description "Tests for the CL-DX-ANIM library"
  :author "Patrick Stein <pat@nklein.com>"
  :version "0.1.20131022"
  :license "UNLICENSE"
  :depends-on (#:cl-dx-anim #:nst)
  :components
  ((:module "src"
    :components ((:static-file "UNLICENSE.txt")
                 (:file "package-t")
                 (:file "data-t" :depends-on ("package-t"))
                 (:file "read-t" :depends-on ("package-t" "data-t"))))))

(defmethod asdf:perform ((op asdf:test-op)
                         (system (eql (asdf:find-system :cl-dx-anim))))
  (asdf:load-system :cl-dx-anim-tests)
  (funcall (find-symbol "RUN-TESTS" :cl-dx-anim-tests)))
