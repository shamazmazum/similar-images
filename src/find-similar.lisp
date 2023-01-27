(in-package :similar-images)

(defun build-tree (images-and-hashes)
  "Build VP-tree on a set of image hashes"
  (make-vp-tree images-and-hashes
                #'hamming-distance
                :key #'cdr))

(defun set-equal-p (set1 set2)
  "Return T if set1 == set2, NIL otherwise"
  (and (subsetp set1 set2 :test #'equalp)
       (subsetp set2 set1 :test #'equalp)))

(defun collect-close (images-and-hashes)
  "Return a list of sets of close enough images"
  (log:info "Searching for similar images")
  (loop with tree = (build-tree images-and-hashes)
        for image-and-hash in images-and-hashes
        for close = (items-in-ball tree image-and-hash
                                   *threshold* #'hamming-distance
                                   :key #'cdr)
        when (> (length close) 1)
        collect (mapcar #'car close) into result
        finally (return (remove-duplicates
                         result
                         :test #'set-equal-p))))

(define-search-function find-similar (directory)
  "Return a list of sets of similar images for images in
@c(directory). If @c(recursive) is @c(T), all subdirectories of
@c(directory) are also scanned for images. If @c(remove-errored) is
@c(T) the pictures which cannot be read are removed. @c(Threshold) is
a sensitivity of algorithm. The bigger the value is the more different
pictures are considered similar."
  (collect-close (collect-hashes directory)))

(define-search-function similar-subset (little big)
    "Find images in the directory @c(little) which are similar to one or
more images in the directory @c(big). For every match a list of
similar images is constructed where the first image belongs to
@c(little) and the rest belong to @c(big). A list of all matches is
returned. If @c(recursive) is @c(T), all subdirectories of
@c(directory) are also scanned for images. If @c(remove-errored) is
@c(T) the pictures which cannot be read are removed. @c(Threshold) is
a sensitivity of algorithm. The bigger the value is the more different
pictures are considered similar."
  (loop with little-hashes = (collect-hashes little)
        with big-hashes = (collect-hashes big)
        with tree = (progn
                      (log:info "Searching for similar images")
                      (make-vp-tree big-hashes #'hamming-distance :key #'cdr))
        for image-and-hash in little-hashes
        for close = (items-in-ball tree image-and-hash
                                   *threshold* #'hamming-distance
                                   :key #'cdr)
        when close
        collect (mapcar #'car (cons image-and-hash close))))
