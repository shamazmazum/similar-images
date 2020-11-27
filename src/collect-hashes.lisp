(in-package :similar-images)

(defparameter *remove-errored* nil
  "Remove an image if the error occured during reading")

(defparameter *recursive* t
  "Do recursive scan for images if T")

(defparameter *image-types* '("jpg" "jpeg" "png")
  "Image file extensions")

(defparameter *use-sqlite* t
  "Use SQLite database backend. NIL is only good for benchmarking
perceptual-hashes.")

(defun handle-condition (c)
  (declare (ignore c))
  (invoke-restart
   (if *remove-errored*
       'remove-file
       'skip-image)))

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
  (with-database (db (make-instance (if *use-sqlite*
                                        'sqlite-database
                                        'dummy-database)
                                    :base-directory directory))
    (insert-new
     db
     (handler-bind
         (((or jpeg-turbo:jpeg-error imago:decode-error)
           #'handle-condition))
       (loop
          with images = (collect-images directory)
          with hashes = (mapcar
                         (lambda (image) (hash db image))
                         images)
          for image in images
          for hash-or-future in hashes
          for hash = (touch hash-or-future)
          when hash
          collect (cons image hash))))))
