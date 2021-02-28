(in-package :similar-images-tests)
(def-suite similar-images :description "Test similar-images")

(defun run-tests ()
  (let ((lparallel:*kernel* (lparallel:make-kernel 2)))
    (every #'identity
           (mapcar (lambda (suite)
                     (let ((status (run suite)))
                       (explain! status)
                       (results-status status)))
                   '(similar-images)))))

(defun get-filename (pathname)
  (declare (type pathname pathname))
  (namestring
   (make-pathname
    :name (pathname-name pathname)
    :type (pathname-type pathname))))

(in-suite similar-images)
(test test-find-similar
  (mapc
   (lambda (func)
     (let ((similar
            (funcall func
                     (asdf:system-relative-pathname :similar-images/tests "tests/set1")
                     :threshold 60)))
       (is (= (length similar) 1))
       (is-true
        (similar-images::set-equal-p
         (mapcar #'get-filename (first similar))
         '("vincent2.jpg" "vincent2-watermark.jpg")))))
   (list #'find-similar #'find-similar-prob)))

(test test-similar-subset
  (let ((similar
         (similar-subset
          (asdf:system-relative-pathname :similar-images/tests "tests/set2")
          (asdf:system-relative-pathname :similar-images/tests "tests/set1")
          :threshold 60)))
    (is (= (length similar) 1))
    (is (equalp
         (mapcar #'get-filename (first similar))
         '("sailor-moon.jpg" "sailor-moon.jpg")))))
