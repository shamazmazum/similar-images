(in-package :similar-images)
(defconstant +hash-length+ 1024
  "Length of a perceptual hash (in bits)")
(defconstant +number-of-bins+ 4
  "Number of sets of bins for classification.")
(defconstant +bin-width+ 11
  "A set of bins will have 2^+bin-width+ bins.")

(defgenerator make-random-gen (max)
  "Make a generator of integer random numbers in the range [0, MAX]
without repetitions. When all numbers are returned, this generator
will return NIL."
  (loop
     with list = (loop for x below max collect x)
     while list
     for random = (nth (random (length list)) list)
     do
       (setq list (delete random list))
       (yield random)))

(defun make-bin-classifier (n &optional (rnd (make-random-gen +hash-length+)))
  "Return bin classifier which designates a bit-vector to one of 2^N
bins. RND is a random number generator."
  (declare (type basic-generator rnd))
  (let ((random-bit-indices (take n rnd)))
    (lambda (vector)
      (declare (optimize (speed 3))
               (type bit-vector vector))
      (loop
         with acc fixnum = 0
         for idx in random-bit-indices
         for bit = (bit vector idx)
         do (setq acc (logior (ash acc 1) bit))
         finally (return acc)))))

(defun make-bins-and-classifiers (n m)
  "Make N bin sets where each set has 2^M places. Also make
corresponding bin classifiers."
  (loop
     with rnd-gen = (make-random-gen +hash-length+)
     repeat n
     collect (cons (make-bin-classifier m rnd-gen)
                   (make-array (ash 1 m) :initial-element nil))))

(defun classify-vector (bins-and-classifiers element
                        &key (key #'identity))
  "Put an ELEMENT to one bin of each bin set. Bin sets and classifiers
are specified in BINS-AND-CLASSIFIERS argument."
  (declare (optimize (speed 3))
           (type function key))
  (dolist (bin-and-classifier bins-and-classifiers)
    (destructuring-bind (classifier . bin)
        bin-and-classifier
      (declare (type function classifier)
               (type simple-vector bin))
      (let ((bin-idx (funcall classifier
                              (funcall key element))))
        (push element (svref bin bin-idx)))))
  bins-and-classifiers)

(defun find-close-prob (bins-and-classifiers element
                        &key (key #'identity))
  "Search bins for all elements which are close enough to ELEMENT."
  (declare (optimize (speed 3))
           (type function key))
  (let (similar)
    (dolist (bin-and-classifier bins-and-classifiers)
      (destructuring-bind (classifier . bin)
          bin-and-classifier
        (declare (type function classifier)
                 (type simple-vector bin))
        (dolist (entry
                  (let ((bin-idx
                         (funcall classifier
                                  (funcall key element))))
                    (svref bin bin-idx)))
          (let ((distance (hamming-distance
                           (funcall key entry)
                           (funcall key element))))
            (declare (type fixnum distance))
            (if (and (not (eq entry element))
                     (<= distance *threshold*))
                (pushnew entry similar
                         :test #'equal
                         :key  #'car))))))
    similar))

(defun find-similar-prob (directory
                          &key
                            (threshold *threshold*)
                            (recursive *recursive*)
                            (remove-errored *remove-errored*)
                            (reporter *reporter*))
  "Return a list of sets of similar images for images in
@c(directory). If @c(recursive) is @c(T), all subdirectories of
@c(directory) are also scanned for images. If @c(remove-errored) is
@c(T) the pictures which cannot be read are removed. @c(Threshold) is
a sensitivity of algorithm. The bigger the value is the more different
pictures are considered similar.
  This is a faster version of @c(find-similar). This algorithm is
probabilistic which means that close images are found only with a
certain degree of probability, e.g. if a distance between images is
45, there is about 2% chance that they will not be recognized as
similar."
  (declare (type (or string pathname) directory)
           (type unsigned-byte threshold))
  (let ((*threshold* threshold)
        (*recursive* recursive)
        (*remove-errored* remove-errored)
        (*reporter* reporter))
    (let ((images-and-hashes (report-state-after "Searching for similar images"
                               (collect-hashes directory)))
          (bins (make-bins-and-classifiers +number-of-bins+
                                           +bin-width+)))
      (mapc
       (lambda (image-and-hash)
         (classify-vector bins image-and-hash :key #'cdr))
       images-and-hashes)
      (loop
         for image-and-hash in images-and-hashes
         for similar = (find-close-prob bins image-and-hash :key #'cdr)
         when similar
         collect (mapcar #'car (cons image-and-hash similar)) into similars
         finally (return (remove-duplicates similars :test #'set-equal-p))))))
