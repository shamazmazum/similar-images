(defsystem :similar-images
  :name :similar-images
  :version "0.1"
  :author "Vasily Postnicov <shamaz.mazum@gmail.com>"
  :description "Find similar images in big datasets"
  :license "2-clause BSD"
  :pathname "src/"
  :serial t
  :components ((:file "package")
               (:file "reporter")
               (:file "classes")
               (:file "methods")
               (:file "collect-hashes")
               (:file "find-similar")
               (:file "find-similar-prob"))
  :in-order-to ((test-op (load-op "similar-images/tests")))
  :perform (test-op (op system)
                    (declare (ignore op system))
                    (uiop:symbol-call :similar-images-tests '#:run-tests))
  :depends-on (:cl-fad
               :perceptual-hashes
               :vp-trees
               :sqlite
               :eager-future2
               :snakes))

(defsystem :similar-images/tests
  :name :similar-images/tests
  :version "0.1"
  :author "Vasily Postnicov <shamaz.mazum@gmail.com>"
  :license "2-clause BSD"
  :pathname "tests/"
  :components ((:file "package")
               (:file "tests" :depends-on ("package")))
  :depends-on (:similar-images :fiveam))

(defsystem :similar-images/misc
  :name :similar-images/misc
  :version "0.1"
  :author "Vasily Postnicov <shamaz.mazum@gmail.com>"
  :license "2-clause BSD"
  :pathname "misc/"
  :serial t
  :components ((:file "packages")
               (:file "remover")
               (:file "viewer"))
  :depends-on (:similar-images :cl-cffi-gtk))

(defsystem :similar-images/cli
  :name :similar-images/cli
  :version "0.1"
  :author "Vasily Postnicov <shamaz.mazum@gmail.com>"
  :license "2-clause BSD"
  :pathname "cli/"
  :serial t
  :components ((:file "package")
               (:file "cli"))
  :depends-on (:similar-images/misc :unix-opts)
  :build-operation program-op
  :build-pathname "similar-images"
  :entry-point "similar-images-cli:main")
