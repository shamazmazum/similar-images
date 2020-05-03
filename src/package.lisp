(defpackage similar-images
  (:use #:cl #:perceptual-hashes #:vp-trees #:fad #:sqlite)
  (:export #:find-similar
           #:similar-subset
           #:*threshold*
           #:*remove-errored*
           #:*resursive*
           #:*image-types*))
