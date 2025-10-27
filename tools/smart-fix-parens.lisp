#!/usr/bin/env sbcl --script

(defun count-parens (line)
  "Count opening and closing parentheses in a line"
  (let ((open-count 0)
        (close-count 0))
    (loop for char across line
          do (cond
               ((char= char #\()
                (incf open-count))
               ((char= char #\))
                (incf close-count))))
    (values open-count close-count)))

(defun fix-paren-balance (lines)
  "Fix parenthesis balance by adding missing closing parentheses"
  (let ((open-count 0)
        (close-count 0)
        (result '()))
    (dolist (line lines)
      (multiple-value-bind (line-open line-close) (count-parens line)
        (incf open-count line-open)
        (incf close-count line-close)
        (push line result)))
    
    ;; Add missing closing parentheses
    (let ((missing-close (- open-count close-count)))
      (when (> missing-close 0)
        (dotimes (i missing-close)
          (push ")" result))))
    
    (reverse result)))

(defun process-file (input-file output-file)
  "Read input file, fix parentheses, write to output file"
  (with-open-file (in input-file :direction :input)
    (let ((lines '()))
      (loop for line = (read-line in nil nil)
            while line
            do (push line lines))
      (let ((fixed-lines (fix-paren-balance (reverse lines))))
        (with-open-file (out output-file :direction :output
                             :if-exists :supersede
                             :if-does-not-exist :create)
          (dolist (line fixed-lines)
            (write-line line out)))))))

;; Main execution
(let ((input-file "src/server/core.lisp")
      (output-file "src/server/core.lisp.fixed"))
  (format t "Smart fixing parentheses in ~A...~%" input-file)
  (process-file input-file output-file)
  (format t "Done! Output written to ~A~%" output-file)
  (format t "Review the file and then: mv ~A ~A~%" output-file input-file))
