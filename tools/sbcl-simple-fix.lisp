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
      (format t "Open parens: ~d, Close parens: ~d, Missing: ~d~%" open-count close-count missing-close)
      (if (> missing-close 0)
          (concatenate 'string content (make-string missing-close :initial-element #\)))
          content))))

(defun fix-file (input-file output-file)
  "Read file, fix parentheses, and write to output file"
  (let ((content (read-file-as-string input-file)))
    (format t "Original content length: ~d~%" (length content))
    
    (let ((fixed-content (fix-paren-balance content)))
      (with-open-file (out output-file :direction :output
                           :if-exists :supersede
                           :if-does-not-exist :create)
        (write-string fixed-content out))
      (format t "Fixed content length: ~d~%" (length fixed-content)))))

;; Main execution
(let ((input-file "src/server/core.lisp")
      (output-file "src/server/core.lisp.fixed"))
  (format t "Using SBCL to fix parentheses in ~A...~%" input-file)
  (fix-file input-file output-file)
  (format t "Done! Output written to ~A~%" output-file)
  (format t "Review the file and then: mv ~A ~A~%" output-file input-file))
