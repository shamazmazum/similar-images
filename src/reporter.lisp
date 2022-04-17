(in-package :similar-images)

(defstruct progress-state
  (last-reported 0 :type number))

(defun report-percentage (state completion)
  (declare (type progress-state state))
  (with-accessors ((last-reported progress-state-last-reported))
      state
    (let ((completion (* 100 (float completion))))
      (when (> (- completion last-reported) 5.0)
        (setf last-reported completion)
        (log:info "~4f% collected" completion)))))
