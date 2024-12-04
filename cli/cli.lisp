(in-package :similar-images-cli)

(defun get-mode (mode)
  (cond
    #-similar-images-no-gui
    ((string= mode "view")   :view)
    ((string= mode "print")  :print)
    ((string= mode "remove") :remove)
    (t (error "Wrong mode"))))

(defun get-ignored-types (string)
  (split-sequence:split-sequence #\, string))

(defparameter *prune-parser*
  (argument :directory "DIRECTORY"))

(defparameter *process-parser*
  (seq
   (optional
    (flag   :recursive
            :short       #\r
            :long        "recursive"
            :description "Search for images recursively")
    (flag   :no-db
            :long        "no-db"
            :description "Do not use the database")
    (option :threads "N"
            :long        "threads"
            :description "Number of threads for hash calculation"
            :fn          #'parse-integer)
    (option :threshold "T"
            :long        "threshold"
            :short       #\t
            :description "Sensitivity of the algorithm (0-1024). Lesser values mean lower sensibility. Good values to try are (40-60)."
            :fn          #'parse-integer)
    (option :hash "HASH"
            :short       #\h
            :long        "hash"
            :description "Hash function to use (can be ahash or dhash)"
            :fn (lambda (hash)
                  (intern
                   (string-upcase hash)
                   (find-package :keyword))))
    (flag   :exhaustive
            :short       #\e
            :long        "exhaustive"
            :description "Run exhaustive search (slow)")
    (flag   :remove-errored
            :long        "remove-errored"
            :description "Remove images which cannot be read (dangerous!)")
    (option :big-set "BIG-DIRECTORY"
            :long        "big-set"
            :description "Specify the big set to match against")
    (option :ignore-types "TYPES"
            :long        "ignore-types"
            :description "List of image types to be ignored, separated by comma"
            :fn          #'get-ignored-types))
   (option :mode "MODE"
            :short       #\m
            :long        "mode"
            :fn          #'get-mode
            :description (format
                          nil "Mode of operation (~{~a~^, ~})"
                          '(#-similar-images-no-gui "view" "print" "remove")))
   (argument :directory "DIRECTORY")))

(defparameter *cmd-parser*
  (seq
   (optional
    (flag :quiet
          :short       #\q
          :long        "quiet"
          :description "Be quiet"))
   (choice
    (seq
     (command :what "prune" :prune
              :description "Remove old entries from the database")
     *prune-parser*)
    (seq
     (command :what "find" :find
              :description "Find similar images in a directory")
     *process-parser*))))

(defun print-usage-and-quit ()
  (print-usage *cmd-parser* "similar-images")
  (uiop:quit 1))

(defun get-arguments-or-fail ()
  (handler-case
      (parse-argv *cmd-parser*)
    (error () (print-usage-and-quit))))

(defun prune (args)
  (prune-database (%assoc :directory args)))

(defun process (args)
  (let* ((big-set    (%assoc :big-set    args))
         (exhaustive (%assoc :exhaustive args))
         (key-args (list :threshold       (%assoc :threshold      args *threshold*)
                         :recursive       (%assoc :recursive      args)
                         :remove-errored  (%assoc :remove-errored args)
                         :hash-function   (%assoc :hash           args *hash-function*)
                         :workers         (%assoc :threads        args *workers*)
                         :use-sqlite (not (%assoc :no-db          args (not *use-sqlite*)))
                         :image-types (set-difference *image-types*
                                                      (%assoc :ignore-types args)
                                                      :test #'string=)))
         (set (%assoc :directory args))
         (similar
          (cond
            (big-set
             (apply #'similar-subset set big-set key-args))
            (exhaustive
             (apply #'find-similar set key-args))
            (t
             (apply #'find-similar-prob set key-args)))))

    (case (%assoc :mode args)
      (:print
       (format t "~a~%" similar))
      #-similar-images-no-gui
      (:view
       (when similar
         (view-images similar (%assoc :directory args))))
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
  (let ((args (get-arguments-or-fail)))
    (log:config
     (if (%assoc :quiet args) :warn :info))
    (ecase (%assoc :what args)
      (:prune (prune   args))
      (:find  (process args))))
  (uiop:quit 0))
