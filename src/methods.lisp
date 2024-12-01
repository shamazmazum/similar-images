(in-package :similar-images)

;; Dummy database
(defmethod hash ((database dummy-database) image)
  (declare (type (or string pathname) image)
           (ignore database))
  (future
    (restart-case
        (funcall (ecase *hash-function*
                   (:ahash #'ahash)
                   (:dhash #'dhash))
                 image)
      (skip-image ()
        :report "Skip this image and continue"
        (values))
      (remove-file ()
        :report "Remove this image"
        (delete-file image)
        (values)))))

;; SQLite database
(defun hash-name (symbol)
  (declare (type perceptual-hash symbol))
  (string-downcase
   (symbol-name symbol)))

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
         db
         #.(concatenate
            'string
            "create table if not exists hashes3 "
            "(name text not null, type text not null, hash blob not null, "
            "unique (name, type));")))
      (setf handle (connect db-pathname)))))

(defmethod close-db ((database sqlite-database))
  (disconnect (db-handle database))
  (values))

(defmethod insert-new ((database sqlite-database) images-and-hashes)
  (let ((handle (db-handle database))
        (base-directory (db-base-directory database)))
    (with-transaction handle
      (mapc
       (lambda (image-and-hash)
         (execute-non-query
          handle
          "insert or ignore into hashes3 (name, type, hash) values (?, ?, ?);"
          (enough-namestring
           (car image-and-hash)
           base-directory)
          (hash-name *hash-function*)
          (cdr image-and-hash)))
       images-and-hashes))))

(defmethod hash ((database sqlite-database) image)
  (declare (type (or pathname string) image))
  (let* ((handle (db-handle database))
         (base-directory (db-base-directory database))
         (relative-path (enough-namestring image base-directory))
         (db-hash (execute-single
                   handle "select hash from hashes3 where name = ? and type = ?"
                   relative-path
                   (hash-name *hash-function*))))
    (if db-hash
        (coerce db-hash 'bit-vector)
        (call-next-method))))

(defmacro with-sql-statement ((statement db sql) &body body)
  `(let ((,statement (prepare-statement ,db ,sql)))
     (unwind-protect
          (progn ,@body)
       (finalize-statement ,statement))))

(defun file-exists-p (filename base-directory)
  "Check if FILENAME is an existing file in BASE-DIRECTORY"
  (probe-file
   (merge-pathnames (pathname filename)
                    (uiop:ensure-directory-pathname base-directory))))

(defmethod remove-missing ((database sqlite-database))
  (let ((handle (db-handle database))
        (base-directory (db-base-directory database)))
    (with-transaction handle
      (let ((entries-to-remove
             (with-sql-statement (st handle "select name from hashes3;")
               (loop while (step-statement st)
                     for name = (statement-column-value st 0)
                     unless (file-exists-p name base-directory)
                     collect name))))
        (loop for entry in entries-to-remove do
              (log:info "Removing stale entry ~a" entry)
              (execute-non-query handle "delete from hashes3 where name = ?"
                                 entry)))))
  (values))
