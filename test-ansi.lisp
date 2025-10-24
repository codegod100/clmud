(defpackage :mud.ansi
  (:use :cl)
  (:export :wrap :code :strip :gradient))

(in-package :mud.ansi)

(defun make-ansi-code (code)
  "Create an ANSI escape sequence"
  (format nil "~C[~Am" (code-char 27) code))

(defparameter *ansi-table* nil
  "ANSI color code lookup table")

(setf *ansi-table*
      (list (cons :reset (make-ansi-code "0"))
            (cons :red (make-ansi-code "31"))
            (cons :bright-red (make-ansi-code "91"))))

(defun %normalize-key (key)
  (etypecase key
    (symbol (string-downcase (symbol-name key)))
    (string (string-downcase key))))

(defun code (name)
  (let* ((key (%normalize-key name))
         (pair (assoc (intern key :keyword) *ansi-table* :test #'eq)))
    (cdr pair)))

(defun wrap (text &rest styles)
  (let ((reset (or (code :reset) "")))
    (with-output-to-string (out)
      (dolist (style styles)
        (let ((seq (code style)))
          (when (and seq (plusp (length seq)))
            (write-string seq out))))
      (write-string text out)
      (write-string reset out))))

;; Tests
(format t "Testing ANSI codes...~%~%")

(format t "Table: ~S~%" *ansi-table*)
(format t "Reset code: ~S~%" (code :reset))
(format t "Red code: ~S~%" (code :red))
(format t "Bright-red code: ~S~%" (code :bright-red))

(let ((result (wrap "Hello" :bright-red)))
  (format t "~%Wrapped string: ~S~%" result)
  (format t "Length: ~D~%" (length result))
  (format t "Display: ~A~%" result))

(format t "~%Direct test: ~C[91mRED~C[0m~%" (code-char 27) (code-char 27))
