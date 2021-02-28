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
  ((no-new-line :initform nil
                :type boolean
                :accessor no-new-line))
  (:documentation "Reporter for CLI tool"))

(defmethod report-state ((reporter cli-reporter) state)
  (when (no-new-line reporter)
    (setf (no-new-line reporter) nil)
    (terpri))
  (format t "~a~%" state))

;; FIXME: works only in VT100 compatible terminals
(defun clear-line ()
  (cond
    ((interactive-stream-p *standard-output*)
     (format t "~c[1K~c"
             #\Esc #\Return))
    (t (terpri))))

(defmethod report-percentage ((reporter cli-reporter) completion)
  (setf (no-new-line reporter) t)
  (clear-line)
  (format t "~4f% complete"
          (float completion)))

(defparameter *reporter* (make-instance 'dummy-reporter)
  "Default reporter")

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
