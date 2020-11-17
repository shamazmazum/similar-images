(defun do-all()
  ;; Check if similar-images/misc is buildable
  (ql:quickload :similar-images/misc)

  (ql:quickload :similar-images/tests)
  (uiop:quit
   (if (uiop:symbol-call
        :similar-images-tests
        '#:run-tests)
       0 1)))

(do-all)
