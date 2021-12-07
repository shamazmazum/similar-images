(in-package :similar-images-tests)
(def-suite similar-images
  :description "Test similar-images")
(def-suite similar-images/remover
  :description "Test similar-images/remover")

(defun run-tests ()
  (let ((*use-sqlite* nil))
    (every #'identity
           (mapcar (lambda (suite)
                     (let ((status (run suite)))
                       (explain! status)
                       (results-status status)))
                   '(similar-images similar-images/remover)))))

(defun get-filename (pathname)
  (declare (type pathname pathname))
  (namestring
   (make-pathname
    :name (pathname-name pathname)
    :type (pathname-type pathname))))

;; Test similar-images
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

;; Test similar-images/remover
(in-suite similar-images/remover)

;; TODO: restore original file if it was accidentaly deleted
(test remove-similar-images
  (let* ((set-directory (asdf:system-relative-pathname
                         :similar-images/tests "tests/set3/"))
         (big-pathname   (merge-pathnames #p"test.jpg"  set-directory))
         (small-pathname (merge-pathnames #p"test2.jpg" set-directory))
         (big-image (imago:read-image big-pathname))
         (small-image (imago:resize big-image 300 100)))
    (imago:write-image small-image small-pathname)
    (find-similar set-directory)
    (let ((result (remove-similar (find-similar set-directory))))
      (is (= (length result) 1))
      (is (equal (first result) small-pathname)))))
