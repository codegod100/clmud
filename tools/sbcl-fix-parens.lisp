#!/usr/bin/env sbcl --script

(defun read-file-as-string (filename)
  "Read entire file as a string"
  (with-open-file (stream filename :direction :input)
    (let ((contents (make-string (file-length stream))))
      (read-sequence contents stream)
      contents)))

(defun count-parens (string)
  "Count opening and closing parentheses in a string"
  (let ((open-count 0)
        (close-count 0))
    (loop for char across string
          do (cond
               ((char= char #\()
                (incf open-count))
               ((char= char #\))
                (incf close-count))))
    (values open-count close-count)))

(defun fix-paren-balance (content)
  "Fix parenthesis balance by adding missing closing parentheses"
  (multiple-value-bind (open-count close-count) (count-parens content)
    (let ((missing-close (- open-count close-count)))
      (if (> missing-close 0)
          (concatenate 'string content (make-string missing-close :initial-element #\)))
          content))))

(defun safe-read-from-string (string)
  "Safely read from string, handling errors gracefully"
  (handler-case
      (read-from-string string)
    (error (e)
      (format t "Error reading s-expression: ~a~%" e)
      nil)))

(defun fix-s-expressions (input-file output-file)
  "Read file, fix parentheses, and write to output file"
  (let ((content (read-file-as-string input-file)))
    (format t "Original content length: ~d~%" (length content))
    
    ;; Try to read as s-expressions first
    (let ((forms (with-input-from-string (stream content)
                   (loop for form = (read stream nil :eof)
                         until (eq form :eof)
                         collect form))))
      (format t "Successfully read ~d forms~%" (length forms))
      
      ;; Pretty print the forms to fix formatting
      (with-open-file (out output-file :direction :output
                           :if-exists :supersede
                           :if-does-not-exist :create)
        (dolist (form forms)
          (pprint form out)
          (terpri out)))))
  
  ;; If that fails, try the simple paren balance fix
  (handler-case
      (let ((content (read-file-as-string input-file)))
        (let ((fixed-content (fix-paren-balance content)))
          (with-open-file (out output-file :direction :output
                               :if-exists :supersede
                               :if-does-not-exist :create)
            (write-string fixed-content out))))
    (error (e)
      (format t "Error in s-expression processing: ~a~%" e))))

;; Main execution
(let ((input-file "src/server/core.lisp")
      (output-file "src/server/core.lisp.fixed"))
  (format t "Using SBCL to fix parentheses in ~A...~%" input-file)
  (fix-s-expressions input-file output-file)
  (format t "Done! Output written to ~A~%" output-file)
  (format t "Review the file and then: mv ~A ~A~%" output-file input-file))
