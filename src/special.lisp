(in-package :similar-images)

(defparameter *reporter* (make-instance 'dummy-reporter)
  "Default reporter")

(defparameter *remove-errored* nil
  "Remove an image if the error occured during reading")

(defparameter *recursive* t
  "Do recursive scan for images if T")

(defparameter *image-types* '("jpg" "jpeg" "png")
  "Image file extensions")

(defparameter *use-sqlite* t
  "Use SQLite database backend. NIL is only good for benchmarking
perceptual-hashes.")

(declaim (type (integer 0 1024) *threshold*))
(defparameter *threshold* 45
  "Sensivity of algorithm. The bigger this value is the more different
images are considered similar. When the value is more than 4096 all
the images are considered similar.")

(defparameter *workers* 6
  "Default number of workers for calculation of hashes")

(deftype perceptual-hash () '(member :ahash :dhash))
(declaim (type perceptual-hash *hash-function*))
(defparameter *hash-function* :ahash
  "Perceptual hash function to use by default (:ahash or :dhash)")

(defmacro define-search-function (name arguments doc &body body)
  "Define search function which does all necessary bindings"
  `(defun ,name ,(append arguments
                  '(&key
                    (image-types *image-types*)
                    (workers *workers*)
                    (threshold *threshold*)
                    (hash-function *hash-function*)
                    (recursive *recursive*)
                    (remove-errored *remove-errored*)
                    (reporter *reporter*)))
     ,doc
     (declare (type perceptual-hash hash-function)
              (type (integer 0 1024) threshold))
     (let ((*threshold* threshold)
           (*hash-function* hash-function)
           (*recursive* recursive)
           (*remove-errored* remove-errored)
           (*reporter* reporter)
           (*image-types* image-types)
           (lparallel:*kernel* (lparallel:make-kernel
                                workers
                                :bindings `((*standard-output* . ,*standard-output*)
                                            (*error-output* . ,*error-output*)
                                            (*hash-function* . ,hash-function)))))
       (unwind-protect
            ,@body
         (lparallel:end-kernel)))))
