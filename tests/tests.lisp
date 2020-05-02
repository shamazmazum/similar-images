(in-package :similar-images-tests)
(def-suite similar-images :description "Test similar-images")

(defun run-tests ()
  (every #'identity
         (mapcar (lambda (suite)
                   (let ((status (run suite)))
                     (explain! status)
                     (results-status status)))
                 '(similar-images))))

(defun get-filename (pathname)
  (declare (type pathname pathname))
  (make-pathname
   :name (pathname-name pathname)
   :type (pathname-type pathname)))

(in-suite similar-images)
(test test-find-similar
  (let ((similar
         (find-similar
          (asdf:system-relative-pathname :similar-images/tests "tests/pictures/")
          :threshold 60)))
    (is (= (length similar) 1))
    (is-true
     (similar-images::set-equal-p
      (mapcar #'get-filename (first similar))
      '(#p"vincent2.jpg" #p"vincent2-watermark.jpg")))))
