(in-package :similar-images)

(declaim (type fixnum *threshold*))
(defparameter *threshold* 45
  "Sensivity of algorithm. The bigger this value is the more different
images are considered similar. When the value is more than 4096 all
the images are considered similar.")

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
  (loop
     with tree = (build-tree images-and-hashes)
     for image-and-hash in images-and-hashes
     for close = (search-close tree image-and-hash
                               *threshold* #'hamming-distance
                               :key #'cdr)
     when (> (length close) 1)
     collect (mapcar #'car close) into result
     finally (return (remove-duplicates result :test #'set-equal-p))))

(defun find-similar (directory
                     &key
                       (threshold *threshold*)
                       (recursive *recursive*)
                       (remove-errored *remove-errored*))
  "Return a list of sets of similar images for images in
@c(directory). If @c(recursive) is @c(T), all subdirectories of
@c(directory) are also scanned for images. If @c(remove-errored) is
@c(T) the pictures which cannot be read are removed. @c(Threshold) is
a sensitivity of algorithm. The bigger the value is the more different
pictures are considered similar."
  (declare (type (or string pathname) directory)
           (type unsigned-byte threshold))
  (let ((*threshold* threshold)
        (*recursive* recursive)
        (*remove-errored* remove-errored))
    (collect-close (collect-hashes directory))))
    
;; O(N^2) case for reference
#+nil
(defun find-similar (directory &optional (threshold *threshold*))
  (declare (type (or string pathname) directory)
           (type unsigned-byte threshold))
  (let ((hashes (collect-hashes directory)))
    (remove-duplicates
     (loop
        for hash1 in hashes
        for close =
          (loop
             for hash2 in hashes
             for dist = (hamming-distance
                         (cdr hash1)
                         (cdr hash2))
             when (< dist threshold)
             collect (cdr hash2))
        when (> (length close) 1)
        collect close)
     :test #'set-equal-p)))

(defun similar-subset (little big
                       &key
                         (threshold *threshold*)
                         (recursive *recursive*)
                         (remove-errored *remove-errored*))
  "Find images in the directory @c(little) which are similar to one or
more images in the directory @c(big). For every match a list of
similar images is constructed where the first image belongs to
@c(little) and the rest belong to @c(big). A list of all matches is
returned. If @c(recursive) is @c(T), all subdirectories of
@c(directory) are also scanned for images. If @c(remove-errored) is
@c(T) the pictures which cannot be read are removed. @c(Threshold) is
a sensitivity of algorithm. The bigger the value is the more different
pictures are considered similar."
  (declare (type (or pathname string) little big))
  (let ((*threshold* threshold)
        (*recursive* recursive)
        (*remove-errored* remove-errored))
    (loop
       with little-hashes = (collect-hashes little)
       with big-hashes = (collect-hashes big)
       with tree = (make-vp-tree big-hashes #'hamming-distance :key #'cdr)
       for image-and-hash in little-hashes
       for close = (search-close tree image-and-hash
                                 *threshold* #'hamming-distance
                                 :key #'cdr)
       when close
       collect (mapcar #'car (cons image-and-hash close)))))
