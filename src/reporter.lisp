(in-package :similar-images)

(defstruct progress-state
  (last-reported      0 :type unsigned-byte)
  (currently-reported 0 :type unsigned-byte)
  (report-each     1000 :type unsigned-byte))

(defun report-processed (state items-processed)
  (declare (type progress-state state)
           (type unsigned-byte items-processed))
  (let ((report-each (progress-state-report-each state)))
    (with-accessors ((last-reported      progress-state-last-reported)
                     (currently-reported progress-state-currently-reported))
        state
      (incf currently-reported items-processed)
      (when (> (- currently-reported last-reported) report-each)
        (setf last-reported currently-reported)
        (log:info "~d files processed" last-reported)))))
