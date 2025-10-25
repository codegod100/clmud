(defpackage :fiveam
  (:use :cl)
  (:export :def-suite :in-suite :test :is :run! :results-status))

(in-package :fiveam)

(defvar *suites* (make-hash-table :test #'eq))
(defvar *current-suite* nil)

(defstruct (results (:constructor make-results (passed failed)))
  passed
  failed)

(defun ensure-suite (name)
  (or (gethash name *suites*)
      (setf (gethash name *suites*)
            (list :tests '()))))

(defmacro def-suite (name &rest parents)
  (declare (ignore parents))
  `(progn
     (ensure-suite ',name)
     ',name))

(defmacro in-suite (name)
  `(setf *current-suite* ',name))

(defmacro test (name &body body)
  (let ((suite (gensym "SUITE")))
    `(let* ((,suite (or *current-suite* ',name))
            (entry (ensure-suite ,suite)))
       (push (cons ',name (lambda () ,@body))
             (getf entry :tests))
       (setf (gethash ,suite *suites*) entry)
       ',name)))

(defmacro is (form &optional message)
  `(unless ,form
     (error (or ,message (format nil "Assertion failed: ~a" ',form)))))

(defun run-suite (suite-name)
  (let* ((entry (ensure-suite suite-name))
         (tests (reverse (getf entry :tests)))
         (passed 0)
         (failed 0))
    (dolist (test tests)
      (handler-case
          (progn
            (funcall (cdr test))
            (incf passed))
        (error (err)
          (incf failed)
          (format t "~&[FAIL] ~a: ~a~%" (car test) err))))
    (format t "~&Suite ~a: ~d passed, ~d failed.~%"
            suite-name passed failed)
    (make-results passed failed)))

(defun run! (&optional (suite-name *current-suite*))
  (let ((results (run-suite (or suite-name :default))))
    (when (zerop (results-failed results))
      (format t "~&All tests passed.~%"))
    results))

(defun results-status (results)
  (zerop (results-failed results)))
