(defpackage similar-images
  (:use #:cl
        #:perceptual-hashes
        #:vp-trees
        #:fad
        #:sqlite
        #:lparallel
        #:snakes)
  (:shadowing-import-from
   #:snakes
   #:chain)
  (:export #:find-similar
           #:find-similar-prob
           #:similar-subset

           #:*threshold*
           #:*remove-errored*
           #:*resursive*
           #:*image-types*
           #:*use-sqlite*
           #:*reporter*

           #:dummy-reporter
           #:cli-reporter))
