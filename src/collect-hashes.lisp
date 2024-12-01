(in-package :similar-images)

(defun handle-condition (c)
  (declare (ignore c))
  (invoke-restart
   (if *remove-errored*
       'remove-file
       'skip-image)))

(defun report-warning (c)
  (log:warn
   "~a"
   (with-output-to-string (out)
     (princ c out)))
  (muffle-warning c))

(defun imagep (pathname)
  "T if pathname designates an image, NIL otherwise"
  (declare (type (or pathname string) pathname))
  (if (find (pathname-type (pathname pathname))
            *image-types*
            :test #'string=)
      pathname))

(defgenerator collect-images (directory)
  "Return a generator generating images in the @c(directory) and, when
*RECURSIVE* is T, its subdirectories."
  (labels ((collect% (directory)
           (let ((files-and-directories
                  (list-directory (pathname-as-directory directory))))
             (dolist (entry files-and-directories)
               (cond
                 ((and *recursive*
                       (directory-pathname-p entry))
                  (collect% entry))
                 ((imagep entry)
                  (yield entry)))))))
    (collect% directory)))

;; Workaround for task-handler-error
(deftype image-error () '(or imago:decode-error jpeg-turbo:jpeg-error))

(defun fmap-cons (function cons)
  (declare (type function function)
           (optimize (speed 3)))
  (cons (car cons)
        (funcall function (cdr cons))))

(defun collect-hashes (directory)
  "Return consed pathname and hash for images in the @c(directory) and
its subdirectories"
  (log:info "Collecting hashes")
  (with-database (db (open-database directory))
    (let ((progress-state (make-progress-state))
          (future-generator (imap (lambda (image)
                                    (cons image (hash db image)))
                                  (collect-images directory))))
      (reduce
       #'nconc
       (task-handler-bind
           ((image-error #'handle-condition)
            (warning     #'report-warning))
         (loop for hash-futures = (take +process-at-once+
                                        future-generator :fail-if-short nil)
               while hash-futures collect
               (insert-new db
                           (loop for name-and-future in hash-futures
                                 for name-and-hash = (fmap-cons #'force name-and-future)
                                 when (cdr name-and-hash) collect name-and-hash))
               do (report-processed
                   progress-state
                   (length hash-futures))))))))

(defun prune-database (directory)
  "Remove database entries for files which are not present on the
filesystem."
  (with-database (db (open-database directory))
    (remove-missing db)))
