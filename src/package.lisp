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

           #:*hash-function*
           #:*threshold*
           #:*remove-errored*
           #:*recursive*
           #:*image-types*
           #:*use-sqlite*
           #:*reporter*
           #:*workers*

           #:dummy-reporter
           #:cli-reporter))
