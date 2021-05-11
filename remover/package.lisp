(defpackage similar-images-remover
  (:use #:cl
        #:similar-images
        #:imago
        #:jpeg-turbo)
  (:export #:get-biggest-image
           #:remove-similar))
