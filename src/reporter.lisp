(in-package :similar-images)

(defclass reporter ()
  ()
  (:documentation "Reporter class for similar-images
library. Inherited classes must provide their methods for
REPORT-STATE and REPORT-PERCENTAGE functions. Not to be
instanciated."))

(defgeneric report-state (reporter state)
  (:documentation "Report that the 'state' of control flow has changed
(e.g. going from collection of images to computation of
hashes). STATE is just a string."))

(defgeneric report-percentage (reporter completion)
  (:documentation "Report completion of similar-image's functions in
percents. COMPLETION is a float or rational number from 0 to 1."))

;; Dummy reporter
(defclass dummy-reporter (reporter)
  ()
  (:documentation "No-op reporter"))

(defmethod report-state ((reporter dummy-reporter) state)
  (declare (ignore reporter state))
  t)

(defmethod report-percentage ((reporter dummy-reporter) completion)
  (declare (ignore reporter completion))
  t)

;; CLI reporter
(defclass cli-reporter (reporter)
  ((last-reported :initform nil
                  :accessor last-reported))
  (:documentation "Reporter for CLI tool"))

(defmethod report-state ((reporter cli-reporter) state)
  (format t "~a~%" state))

(defmethod report-percentage ((reporter cli-reporter) completion)
  (with-accessors ((last-reported last-reported))
      reporter
    (let ((completion (* 100 (float completion))))
      (when (or (not last-reported)
                (> (- completion last-reported) 5.0))
        (setf last-reported completion)
        (format t "~4f%~%" completion)))))

;; Helper macros
(defmacro report-state-after (state &body body)
  "Execute a code and report the state after that"
  `(prog1 (progn ,@body)
     (report-state *reporter* ,state)))

(defmacro report-state-before (state &body body)
  "Execute a code but report the state before that"
  `(progn
     (report-state *reporter* ,state)
     ,@body))

(defmacro report-percentage% (completion &body body)
  "Execute a code and report percentage"
  `(prog1 (progn ,@body)
     (report-percentage *reporter* ,completion)))
