(defsystem :similar-images
    :name :similar-images
    :version "0.1"
    :author "Vasily Postnicov <shamaz.mazum@gmail.com>"
    :description "Find similar images in big datasets"
    :license "2-clause BSD"
    :serial t
    :components ((:file "src/package")
                 (:file "src/classes")
                 (:file "src/methods")
                 (:file "src/collect-hashes")
                 (:file "src/find-similar")
                 (:file "src/find-similar-prob"))
    :in-order-to ((test-op (load-op "similar-images/tests")))
    :perform (test-op (op system)
                      (declare (ignore op system))
                      (funcall
                       (symbol-function
                        (intern (symbol-name '#:run-tests)
                                (find-package :similar-images-tests)))))
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
  :components ((:file "tests/package")
               (:file "tests/tests" :depends-on ("tests/package")))
  :depends-on (:similar-images :fiveam))

(defsystem :similar-images/misc
    :name :similar-images/misc
    :version "0.1"
    :author "Vasily Postnicov <shamaz.mazum@gmail.com>"
    :license "2-clause BSD"
    :serial t
    :components ((:file "misc/packages")
                 (:file "misc/remover")
                 (:file "misc/viewer"))
  :depends-on (:similar-images :cl-cffi-gtk))
