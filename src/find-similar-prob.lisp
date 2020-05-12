(in-package :similar-images)
(defconstant +hash-length+ 1024
  "Length of a perceptual hash (in bits)")
(defconstant +number-of-bins+ 4
  "Number of sets of bins for classification.")
(defconstant +bin-width+ 11
  "A set of bins will have 2^+bin-width+ bins.")

(defun make-random-gen (max)
  "Make a generator of integer random numbers in the range [0, MAX]
without repetitions. When all numbers are returned, this generator
will return NIL."
  (let ((list (loop for x below max collect x)))
    (lambda ()
      (if list
          (let ((random (nth (random (length list))
                             list)))
            (setq list (delete random list))
            random)))))

(defun make-bin-classifier (n &optional (rnd (make-random-gen +hash-length+)))
  "Return bin classifier which designates a bit-vector to one of 2^N
bins. RND is a random number generator."
  (declare (type function rnd))
  (let ((random-bit-indices (loop for x = (funcall rnd) repeat n
                               collect x)))
    (lambda (vector)
      (declare (type bit-vector vector))
      (loop
         with acc = 0
         for idx in random-bit-indices
         for bit = (bit vector idx)
         do (setq acc (+ (ash acc 1) bit))
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
  (loop
     for bin-and-classifier in bins-and-classifiers
     for bin-idx = (funcall (car bin-and-classifier)
                            (funcall key element))
     do
       (push element
             (aref (cdr bin-and-classifier)
                   bin-idx)))
  bins-and-classifiers)

(defun find-close-prob (bins-and-classifiers element
                        &key (key #'identity))
  "Search bins for all elements which are close enough to ELEMENT."
  (let (similar)
    (loop
       for bin-and-classifier in bins-and-classifiers
       for bin-idx = (funcall (car bin-and-classifier)
                              (funcall key element))
       do
         (loop
            for entry in (aref (cdr bin-and-classifier) bin-idx)
            for distance = (hamming-distance
                            (funcall key entry)
                            (funcall key element))
            when (and (not (eq entry element))
                      (<= distance *threshold*))
            do (pushnew entry similar)))
    similar))

(defun find-similar-prob (directory
                          &key
                            (threshold *threshold*)
                            (recursive *recursive*)
                            (remove-errored *remove-errored*))
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
        (*remove-errored* remove-errored))
    (let ((images-and-hashes (collect-hashes directory))
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
