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
                 (:file "src/find-similar"))
    :depends-on (:cl-fad :perceptual-hashes :vp-trees :sqlite))
