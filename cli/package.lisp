(defpackage similar-images-cli
  (:use #:cl
        #:similar-images
        #-similar-images-no-gui
        #:similar-images-viewer-sdl
        #:similar-images-remover
        #:command-line-parse)
  (:export #:main))
