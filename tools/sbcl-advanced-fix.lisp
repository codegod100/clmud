#!/usr/bin/env sbcl --script

;; Load the packages first
(load "src/packages.lisp")
(load "src/world.lisp")
(load "src/player.lisp")
(load "src/inventory.lisp")
(load "src/mob.lisp")
(load "src/combat.lisp")
(load "src/quest.lisp")
(load "src/merchant.lisp")

(defun read-file-as-string (filename)
  "Read entire file as a string"
  (with-open-file (stream filename :direction :input)
    (let ((contents (make-string (file-length stream))))
      (read-sequence contents stream)
      contents)))

(defun fix-s-expressions (input-file output-file)
  "Read file, parse as s-expressions, and write back with proper formatting"
  (let ((content (read-file-as-string input-file)))
    (format t "Original content length: ~d~%" (length content))
    
    ;; Try to read as s-expressions
    (handler-case
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
              (terpri out))))
      (error (e)
        (format t "Error reading s-expressions: ~a~%" e)
        ;; Fall back to simple paren counting
        (let ((open-count 0)
              (close-count 0))
          (loop for char across content
                do (cond
                     ((char= char #\()
                      (incf open-count))
                     ((char= char #\))
                      (incf close-count))))
          (let ((missing-close (- open-count close-count)))
            (format t "Open parens: ~d, Close parens: ~d, Missing: ~d~%" open-count close-count missing-close)
            (let ((fixed-content (if (> missing-close 0)
                                     (concatenate 'string content (make-string missing-close :initial-element #\)))
                                     content)))
              (with-open-file (out output-file :direction :output
                                   :if-exists :supersede
                                   :if-does-not-exist :create)
                (write-string fixed-content out))))))))

;; Main execution
(let ((input-file "src/server/core.lisp")
      (output-file "src/server/core.lisp.fixed"))
  (format t "Using SBCL to fix s-expressions in ~A...~%" input-file)
  (fix-s-expressions input-file output-file)
  (format t "Done! Output written to ~A~%" output-file)
  (format t "Review the file and then: mv ~A ~A~%" output-file input-file))
