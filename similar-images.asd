(defsystem :similar-images
  :name :similar-images
  :version "0.2"
  :author "Vasily Postnicov <shamaz.mazum@gmail.com>"
  :description "Find similar images in big datasets"
  :license "2-clause BSD"
  :pathname "src/"
  :serial t
  :components ((:file "package")
               (:file "reporter")
               (:file "special")
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
               :log4cl
               :perceptual-hashes
               :vp-trees
               :sqlite
               :lparallel
               :snakes))

#-similar-images-no-gui
(defsystem :similar-images/viewer
  :name :similar-images/viewer
  :version "0.2"
  :author "Vasily Postnicov <shamaz.mazum@gmail.com>"
  :license "2-clause BSD"
  :pathname "viewer/"
  :serial t
  :components ((:file "package")
               (:file "gdk-hackery")
               (:file "viewer"))
  :depends-on (:similar-images
               :cl-cffi-gtk))

(defsystem :similar-images/remover
  :name :similar-images/remover
  :version "0.2"
  :author "Vasily Postnicov <shamaz.mazum@gmail.com>"
  :license "2-clause BSD"
  :pathname "remover/"
  :serial t
  :components ((:file "package")
               (:file "remover"))
  :depends-on (:similar-images
               :rutils
               :imago/pngload
               :imago/jpeg-turbo))

(defsystem :similar-images/misc
  :name :similar-images/misc
  :version "0.2"
  :author "Vasily Postnicov <shamaz.mazum@gmail.com>"
  :license "2-clause BSD"
  :depends-on (:similar-images
               #-similar-images-no-gui
               :similar-images/viewer
               :similar-images/remover))

(defsystem :similar-images/tests
  :name :similar-images/tests
  :version "0.2"
  :author "Vasily Postnicov <shamaz.mazum@gmail.com>"
  :license "2-clause BSD"
  :pathname "tests/"
  :serial t
  :components ((:file "package")
               (:file "tests"))
  :depends-on (:similar-images
               :similar-images/remover
               :fiveam))

(defsystem :similar-images/cli
  :name :similar-images/cli
  :version "0.2"
  :author "Vasily Postnicov <shamaz.mazum@gmail.com>"
  :license "2-clause BSD"
  :pathname "cli/"
  :serial t
  :components ((:file "package")
               (:file "cli"))
  :depends-on (:similar-images/misc
               :unix-opts
               :rutils)
  :build-operation program-op
  :build-pathname "similar-images"
  :entry-point "similar-images-cli:main")
