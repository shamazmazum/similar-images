(in-package :similar-images-remover)

(defun get-dimensions-jpeg (image)
  (declare (type (or string pathname) image))
  (multiple-value-bind (width height)
      (with-decompressor (handle)
        (decompress-header handle image))
    (list width height)))

(defun get-dimensions-rest (image)
  (declare (type (or string pathname) image))
  (let ((image (read-image image)))
    (list
     (image-width image)
     (image-height image))))

(defparameter *dimension-getters*
  `(("jpg"  . ,#'get-dimensions-jpeg)
    ("jpeg" . ,#'get-dimensions-jpeg))
  "Functions which return dimensions based on the image's type")

(defun get-dimensions (image)
  "Get a list containing dimensions of an image. The full
decompression is avoided when possible"
  (declare (type (or string pathname) image))
  (let ((type (pathname-type (pathname image))))
    (funcall
     (rutils:assoc1 type *dimension-getters*
                    :test    #'string=
                    :default #'get-dimensions-rest)
     image)))

(defun get-biggest-image (images)
  "Get biggest image in the list @c(images) judging by its area."
  (flet ((area (image)
           (apply #'* (get-dimensions image))))
    (reduce (lambda (image1 image2)
              (if (> (area image1)
                     (area image2))
                  image1 image2))
            images)))

(defun remove-similar (images &key (best-criterion #'get-biggest-image) dry-run)
  "Remove similar images. This function takes matches returned by
@c(find-similar) or @(find-similar-prob) and removes all images with
exception of one from all matches. Remaining images are chosen by
@c(best-criterion) function.

The list of removed images is returned. If @c(dry-run) is @c(T), the
images are not removed, and this function only produces a list of
images which you may wish to remove."
  (mapc
   (if dry-run #'identity #'delete-file)
   ;; There can be duplicates because one image can belong to multiple
   ;; groups, i.e. ρ(x, y) < c and ρ(y, z) < c does not mean
   ;; ρ(x, z) < c.
   (remove-duplicates
    (reduce
     (lambda (for-deletion group)
       (append (remove
                (funcall best-criterion group)
                group :test #'equal)
               for-deletion))
     images :initial-value nil)
    :test #'equal)))
