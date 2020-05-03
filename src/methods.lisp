(in-package :similar-images)

;; Dummy database
(defmethod hash ((database dummy-database) image)
  (declare (type (or string pathname) image)
           (ignore database))
  (restart-case (ahash image)
    (continue ()
      :report "Skip this image and continue"
      (values))
    (remove-file ()
      :report "Remove this image"
      (delete-file image)
      (values))))

;; SQLite database
(defmethod initialize-instance :after ((database sqlite-database) &rest args)
  (declare (ignore args))
  (with-accessors ((base-directory db-base-directory)
                   (handle db-handle))
      database
    (setf base-directory (truename base-directory))
    (let ((db-pathname (merge-pathnames base-directory #p"hashes.db")))
      ;; Make sure db exists
      (open db-pathname
            :direction :probe
            :if-does-not-exist :create)
      (with-open-database (db db-pathname)
        (execute-non-query
         db "create table if not exists hashes (name text not null unique, hash blob not null);"))
      (setf handle (connect db-pathname)))))

(defmethod close-db ((database sqlite-database))
  (disconnect (db-handle database)))

(defmethod insert-new ((database sqlite-database) images-and-hashes)
  (let ((handle (db-handle database))
        (base-directory (db-base-directory database)))
    (with-transaction handle
      (mapc
       (lambda (image-and-hash)
         (execute-non-query
          handle
          "insert or ignore into hashes (name, hash) values (?, ?);"
          (enough-namestring
           (car image-and-hash)
           base-directory)
          (cdr image-and-hash)))
       images-and-hashes))))

(defmethod hash ((database sqlite-database) image)
  (declare (type (or pathname string) image))
  (let* ((handle (db-handle database))
         (base-directory (db-base-directory database))
         (relative-path (enough-namestring image base-directory))
         (db-hash (execute-single
                   handle "select hash from hashes where name = ?"
                   relative-path)))

    (if db-hash
        (coerce db-hash 'bit-vector)
        (call-next-method))))
