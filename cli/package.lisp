(defpackage similar-images-cli
  (:use #:cl
        #:similar-images
        #-similar-images-no-gui
        #:similar-images-viewer
        #:similar-images-remover)
  (:export #:main))
