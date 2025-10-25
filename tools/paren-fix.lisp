#!/usr/bin/env sbcl --script
;;; Simple parenthesis balance checker and fixer for the MUD codebase.
;;; The fixer is lexical: it only counts literal '(' and ')' characters.

(defun read-file-as-string (path)
  (with-open-file (stream path :direction :input)
    (let ((contents (make-string (file-length stream))))
      (read-sequence contents stream)
      contents)))

(defun write-string-to-file (path content)
  (with-open-file (stream path :direction :output :if-exists :supersede)
    (write-sequence content stream)))

(defun analyze-parens (text)
  "Return opens, closes, max depth, missing closing count, and positions of extra closes."
  (let ((opens 0)
        (closes 0)
        (depth 0)
        (max-depth 0)
        (line 1)
        (column 0)
        (extra-closes '()))
    (loop for ch across text do
          (cond
            ((char= ch #\Newline)
             (incf line)
             (setf column 0))
            (t
             (incf column)
             (cond
               ((char= ch #\()
                (incf opens)
                (incf depth)
                (setf max-depth (max max-depth depth)))
               ((char= ch #\))
                (incf closes)
                (if (> depth 0)
                    (decf depth)
                    (push (cons line column) extra-closes)))))))
    (let ((missing (max 0 (- opens closes))))
      (values opens closes max-depth missing (nreverse extra-closes)))))

(defun auto-fix-content (text)
  "Remove unmatched closing parens and append missing closing parens."
  (let ((line 1)
        (column 0)
        (depth 0)
        (removed '())
        (out (make-string-output-stream)))
    (loop for ch across text do
          (cond
            ((char= ch #\Newline)
             (write-char ch out)
             (incf line)
             (setf column 0))
            (t
             (incf column)
             (cond
               ((char= ch #\()
                (incf depth)
                (write-char ch out))
               ((char= ch #\))
                (if (> depth 0)
                    (progn
                      (decf depth)
                      (write-char ch out))
                    (push (cons line column) removed)))
               (t
                (write-char ch out))))))
    (let ((missing depth))
      (loop repeat missing do (write-char #\) out))
      (values (get-output-stream-string out)
              (nreverse removed)
              missing))))

(defun print-analysis (file)
  (let* ((contents (read-file-as-string file)))
    (multiple-value-bind (opens closes max-depth missing extra)
        (analyze-parens contents)
      (format t "~&File: ~A~%" file)
      (format t "  Opens:   ~D~%" opens)
      (format t "  Closes:  ~D~%" closes)
      (format t "  Max depth observed: ~D~%" max-depth)
      (cond
        ((and (zerop missing) (null extra))
         (format t "  Status: balanced~%"))
        (t
         (format t "  Status: UNBALANCED (diff ~D)~%" (- opens closes))
         (when (plusp missing)
           (format t "  Missing closing parens: ~D~%" missing))
         (when extra
           (format t "  Extra closing parens at:~%")
           (dolist (pos extra)
             (format t "    line ~D, column ~D~%" (car pos) (cdr pos)))))))))

(defun fix-file (input output)
  (let* ((contents (read-file-as-string input)))
    (multiple-value-bind (fixed removed appended)
        (auto-fix-content contents)
      (write-string-to-file output fixed)
      (format t "Wrote balanced contents to ~A~%" output)
      (if (and (= (length removed) 0) (zerop appended))
          (format t "No changes were necessary.~%")
          (progn
            (when removed
              (format t "Removed ~D extra closing parens:~%" (length removed))
              (dolist (pos removed)
                (format t "  line ~D, column ~D~%" (car pos) (cdr pos))))
            (when (plusp appended)
              (format t "Appended ~D closing parens at EOF.~%" appended)))))))

(defun show-usage ()
  (format t "Paren fixer~%")
  (format t "Usage:~%")
  (format t "  sbcl --script tools/paren-fix.lisp check <file>~%")
  (format t "  sbcl --script tools/paren-fix.lisp fix <file> <output-file>~%")
  (format t "  sbcl --script tools/paren-fix.lisp fix <file> --in-place~%"))

(defun main ()
  (let ((args (rest sb-ext:*posix-argv*)))
    (unless args
      (show-usage)
      (sb-ext:exit :code 1))
    (let ((command (string-downcase (first args)))
          (rest (rest args)))
      (cond
        ((string= command "check")
         (unless (= (length rest) 1)
           (show-usage)
           (sb-ext:exit :code 1))
         (print-analysis (first rest)))
        ((string= command "fix")
         (unless rest
           (show-usage)
           (sb-ext:exit :code 1))
         (let ((input (first rest))
               (target (second rest)))
           (cond
             ((null target)
              (show-usage)
              (sb-ext:exit :code 1))
             ((string= target "--in-place")
              (fix-file input input))
             (t
              (fix-file input target)))))
        (t
         (format t "Unknown command: ~A~%" command)
         (show-usage)
         (sb-ext:exit :code 1))))))

(main)
