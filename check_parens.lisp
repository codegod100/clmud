#!/usr/bin/env sbcl --script

(defun count-parens (text)
  "Count parentheses in text. Returns (opens . closes)"
  (let ((opens 0)
        (closes 0))
    (loop for char across text do
          (case char
            (#\( (incf opens))
            (#\) (incf closes))))
    (cons opens closes)))

(defun check-file (filename)
  "Check parenthesis balance in a file"
  (format t "~%Checking ~A...~%" filename)
  (with-open-file (stream filename :direction :input)
    (let ((content (make-string (file-length stream))))
      (read-sequence content stream)
      (let ((counts (count-parens content)))
        (format t "  Opens:  ~D~%" (car counts))
        (format t "  Closes: ~D~%" (cdr counts))
        (if (= (car counts) (cdr counts))
            (format t "  Status: BALANCED~%")
            (format t "  Status: UNBALANCED (diff: ~D)~%" 
                    (- (car counts) (cdr counts))))
        (= (car counts) (cdr counts))))))

;; Try to read forms from file
(defun read-all-forms (filename)
  "Attempt to read all top-level forms from file"
  (format t "~%Reading forms from ~A...~%" filename)
  (handler-case
      (with-open-file (stream filename :direction :input)
        (let ((form-count 0)
              (last-good-position 0))
          (handler-case
              (loop
                (let ((pos (file-position stream)))
                  (setf last-good-position pos)
                  (let ((form (read stream nil 'eof)))
                    (when (eq form 'eof)
                      (format t "  Successfully read ~D forms~%" form-count)
                      (return t))
                    (incf form-count))))
            (error (e)
              (format t "  ERROR after ~D forms at position ~D: ~A~%" 
                      form-count last-good-position e)
              nil))))
    (error (e)
      (format t "  ERROR opening file: ~A~%" e)
      nil)))

(let ((files '("src/server.lisp" "src/combat.lisp" "src/inventory.lisp")))
  (dolist (file files)
    (check-file file)
    (read-all-forms file)))
