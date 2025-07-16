(defpackage similar-images
  (:use #:cl
        #:perceptual-hashes
        #:fad
        #:sqlite
        #:lparallel
        #:snakes)
  (:local-nicknames (#:vp #:vp-trees))
  (:shadowing-import-from
   #:snakes
   #:chain)
  (:export #:find-similar
           #:find-similar-prob
           #:similar-subset
           #:prune-database

           #:*hash-function*
           #:*threshold*
           #:*remove-errored*
           #:*recursive*
           #:*image-types*
           #:*use-sqlite*
           #:*workers*))
