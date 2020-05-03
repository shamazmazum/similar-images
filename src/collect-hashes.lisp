(in-package :similar-images)

(defparameter *remove-errored* nil
  "Remove an image if the error occured during reading")

(defparameter *recursive* t
  "Do recursive scan for images if T")

(defparameter *image-types* '("jpg" "jpeg" "png")
  "Image file extensions")

(defun handle-condition (c)
  (declare (ignore c))
  (if *remove-errored*
      (invoke-restart 'remove-file)
      (continue)))

(defun imagep (pathname)
  "T if pathname designates an image, NIL otherwise"
  (declare (type (or pathname string) pathname))
  (if (find (pathname-type (pathname pathname))
            *image-types*
            :test #'string=)
      pathname))

(defun collect-images (directory)
  "Return a list of images in the @c(directory) and its subdirectories"
  (let (files)
    (labels ((collect-files% (directory)
               (let ((files-and-directories
                      (list-directory (pathname-as-directory directory))))
                 (mapc
                  (lambda (file-or-directory)
                    (cond
                      ((and *recursive*
                            (directory-pathname-p file-or-directory))
                       (collect-files% file-or-directory))
                      ((imagep file-or-directory)
                       (push file-or-directory files))))
                  files-and-directories))))
      (collect-files% directory))
    files))

(defun collect-hashes (directory)
  "Return consed pathname and hash for images in the @c(directory) and
its subdirectories"
  (with-database (db (make-instance 'sqlite-database
                                    :base-directory directory))
    (insert-new
     db
     (handler-bind
         ;; FIXME: does imago have its own conditions?
         ((jpeg-turbo:jpeg-error #'handle-condition))
       (loop
          for image in (collect-images directory)
          for hash = (hash db image)
          when hash collect (cons image hash))))))
