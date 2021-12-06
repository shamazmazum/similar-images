(in-package :similar-images)

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
  (report-state-before "Collecting images"
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
      files)))

;; Workaround for task-handler-error
(deftype image-error () '(or imago:decode-error jpeg-turbo:jpeg-error))

(defun collect-hashes (directory)
  "Return consed pathname and hash for images in the @c(directory) and
its subdirectories"
  (with-database (db (make-instance (if *use-sqlite*
                                        'sqlite-database
                                        'dummy-database)
                                    :base-directory directory))
    (insert-new
     db
     (task-handler-bind
         ((image-error #'handle-condition))
       (loop
          with images = (report-state-after "Collecting hashes"
                          (collect-images directory))
          with hashes = (mapcar
                         (lambda (image) (hash db image))
                         images)
          for image in images
          for counter from 0 by 1
          for hash-or-future in hashes
          for hash = (force hash-or-future)
          when hash collect
            (cons image hash)
          do
            (report-percentage
             *reporter*  (/ counter (length images))))))))
