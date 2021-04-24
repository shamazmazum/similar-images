(in-package :similar-images-cli)

(defun get-mode (mode)
  (cond
    ((string= mode "view")   :view)
    ((string= mode "print")  :print)
    ((string= mode "remove") :remove)
    (t (error "Wrong mode"))))

(defun get-ignored-types (string)
  (split-sequence:split-sequence
   #\, string))

(opts:define-opts
  (:name :mode
   :description "Mode of operation (view, print or remove)"
   :short #\m
   :long "mode"
   :meta-var "MODE"
   :arg-parser #'get-mode)
  (:name :quiet
   :description "Be quiet"
   :short #\q
   :long "quiet")
  (:name :recursive
   :description "Search for images recursively"
   :short #\r
   :long "recursive")
  (:name :threads
   :description "Number of threads"
   :long "threads"
   :meta-var "THREADS"
   :arg-parser #'parse-integer)
  (:name :threshold
   :description "Sensitivity of the algorithm (0-1024). Lesser values
mean lower sensibility. Good values to try are (40-60)."
   :short #\t
   :long "threshold"
   :meta-var "THRESHOLD"
   :arg-parser #'parse-integer)
  (:name :hash
   :description "Hash function to use (can be ahash or dhash)"
   :short #\h
   :long "hash"
   :meta-var "HASH"
   :arg-parser (lambda (hash)
                 (intern
                  (string-upcase hash)
                  (find-package :keyword))))
  (:name :remove-errored
   :description "Remove images which cannot be read (dangerous!)"
   :long "remove-errored")
  (:name :big-set
   :description "Specify the big set to match against"
   :long "big-set"
   :meta-var "BIG-DIRECTORY"
   :arg-parser #'identity)
  (:name :ignore-types
   :long "ignore-types"
   :description "List of image types to be ignored, separated by comma"
   :meta-var "TYPES"
   :arg-parser #'get-ignored-types))

(defun print-usage-and-quit ()
  (opts:describe :usage-of "similar-images"
                 :args "DIRECTORY")
  (uiop:quit 1))

(defun do-all-stuff (options arguments)
  (when (/= (length arguments) 1)
    (print-usage-and-quit))
  (let* ((big-set (getf options :big-set))
         (set (first arguments))
         (key-args (list :threshold      (getf options :threshold      *threshold*)
                         :recursive      (getf options :recursive      *recursive*)
                         :remove-errored (getf options :remove-errored *remove-errored*)
                         :hash-function  (getf options :hash           *hash-function*)
                         :workers        (getf options :threads        *workers*)
                         :reporter   (if (getf options :quiet)
                                         (make-instance 'dummy-reporter)
                                         (make-instance 'cli-reporter))
                         :image-types    (set-difference *image-types*
                                                         (getf options :ignore-types)
                                                         :test #'string=)))
         (similar (if big-set
                      (apply #'similar-subset set big-set key-args)
                      (apply #'find-similar-prob set key-args))))

    (case (getf options :mode :view)
      (:print
       (format t "~a~%" similar))
      (:view
       (view similar)
       (gtk:join-gtk-main))
      (:remove
       (remove-similar similar)))))

(defun set-signal-handlers ()
  ;; Tried trivial-signal without success
  ;; At least sbcl is covered
  #+(and sbcl (not win32))
  (sb-unix::enable-interrupt
   sb-unix:sigint :default))

(defun main ()
  (set-signal-handlers)
  (handler-bind
      ((opts:troublesome-option
         (lambda (c)
           (declare (ignore c))
           (print-usage-and-quit))))
    (multiple-value-bind (options arguments)
        (opts:get-opts)
      (do-all-stuff options arguments)
      (uiop:quit 0))))
