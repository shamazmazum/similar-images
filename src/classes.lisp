(in-package :similar-images)

(defclass hash-database ()
  ((base-directory :initarg       :base-directory
                   :initform      (error "Specify base directory")
                   :accessor      db-base-directory
                   :type          (or pathname string)
                   :documentation "Parent directory for all images"))
  (:documentation "Class for database of image hashes. Not to be
instantiated."))

(defclass dummy-database (hash-database)
  ()
  (:documentation "Dummy database class. Objects of this class
calculate perceptional hashes but don't provide any storage for them."))

(defclass sqlite-database (dummy-database hash-database)
  ((handle         :accessor db-handle))
  (:documentation "SQLite backend for hashes database"))

(defgeneric hash (database image)
  (:documentation "Return hash for an image @c(image)"))

(defgeneric insert-new (database images-and-hashes)
  (:documentation "Insert new images and hashes in the database")
  (:method ((database hash-database) images-and-hashes)
    images-and-hashes))

(defgeneric close-db (database)
  (:documentation "Close database")
  (:method ((database hash-database))
    (values)))

(defgeneric remove-missing (database)
  (:documentation "Remove entries which are missing from the base directory")
  (:method ((database hash-database))
    (values)))

(defmacro with-database ((database init-form) &body body)
  "Execute @c(body) in the context on open database
@c(database) which is initialized with @c(init-form). Safely close the
database when control leaves @c(body)." 
  `(let ((,database ,init-form))
     (unwind-protect
          (progn ,@body)
       (close-db ,database))))

(defun open-database (base-directory)
  "Open a database. If *USE-SQLITE* is NIL, a dummy database is used."
  (make-instance
   (if *use-sqlite*
       'sqlite-database
       'dummy-database)
   :base-directory base-directory))
