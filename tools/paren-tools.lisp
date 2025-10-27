#!/usr/bin/env sbcl --script
;;; Comprehensive Parenthesis Tools for CLMUD
;;; Consolidates all parenthesis checking and fixing functionality

(defun read-file-as-string (path)
  "Read entire file as a string"
  (with-open-file (stream path :direction :input)
    (let ((contents (make-string (file-length stream))))
      (read-sequence contents stream)
      contents)))

(defun write-string-to-file (path content)
  "Write string content to file"
  (with-open-file (stream path :direction :output :if-exists :supersede)
    (write-sequence content stream)))

(defun analyze-parens-detailed (text)
  "Analyze parentheses with detailed line-by-line tracking, handling strings and comments"
  (let ((opens 0)
        (closes 0)
        (depth 0)
        (max-depth 0)
        (line 1)
        (column 0)
        (in-string nil)
        (in-comment nil)
        (escape-next nil)
        (unmatched-closes '()))
    
    (loop for char across text do
          (cond
            ;; Handle escape sequences in strings
            (escape-next
             (setf escape-next nil))
            
            ;; Handle strings
            ((and (char= char #\") (not in-comment))
             (setf in-string (not in-string)))
            
            ;; Handle line comments (only when not in string)
            ((and (char= char #\;) (not in-string) (not in-comment))
             (setf in-comment t))
            
            ;; Handle newlines
            ((char= char #\Newline)
             (setf in-comment nil)
             (incf line)
             (setf column 0))
            
            ;; Count parentheses only when not in string or comment
            ((and (not in-string) (not in-comment))
             (incf column)
             (cond
               ((char= char #\()
                (incf opens)
                (incf depth)
                (setf max-depth (max max-depth depth)))
               ((char= char #\))
                (incf closes)
                (if (> depth 0)
                    (decf depth)
                    (push (cons line column) unmatched-closes)))))
            
            ;; Regular character
            (t
             (incf column))))
    
    (let ((missing (max 0 (- opens closes))))
      (values opens closes max-depth missing (nreverse unmatched-closes)))))

(defun show-running-depth (text)
  "Show running parenthesis depth through text"
  (let ((depth 0)
        (line 1)
        (column 0)
        (in-string nil)
        (in-comment nil)
        (escape-next nil))
    (loop for char across text do
          (cond
            ;; Handle escape sequences in strings
            (escape-next
             (setf escape-next nil))
            
            ;; Handle strings
            ((and (char= char #\") (not in-comment))
             (setf in-string (not in-string)))
            
            ;; Handle line comments (only when not in string)
            ((and (char= char #\;) (not in-string) (not in-comment))
             (setf in-comment t))
            
            ;; Handle newlines
            ((char= char #\Newline)
             (setf in-comment nil)
             (incf line)
             (setf column 0))
            
            ;; Count parentheses only when not in string or comment
            ((and (not in-string) (not in-comment))
             (incf column)
             (let ((old-depth depth))
               (cond
                 ((char= char #\()
                  (incf depth))
                 ((char= char #\))
                  (decf depth)))
               ;; Report significant depth changes
               (when (and (not (= old-depth depth))
                          (or (= depth 0) 
                              (< depth 0) 
                              (> (abs (- depth old-depth)) 2)))
                 (format t "Line ~D: depth ~D -> ~D~%" line old-depth depth))))))
    (format t "~%Final depth: ~D~%" depth)))

(defun auto-fix-content (text)
  "Remove unmatched closing parens and append missing closing parens.
   Handles strings and comments properly."
  (let ((line 1)
        (column 0)
        (depth 0)
        (in-string nil)
        (in-comment nil)
        (escape-next nil)
        (removed '())
        (out (make-string-output-stream)))
    (loop for ch across text do
          (cond
            ;; Handle escape sequences in strings
            (escape-next
             (write-char ch out)
             (setf escape-next nil))
            
            ;; Handle strings
            ((and (char= ch #\") (not in-comment))
             (write-char ch out)
             (setf in-string (not in-string)))
            
            ;; Handle line comments (only when not in string)
            ((and (char= ch #\;) (not in-string) (not in-comment))
             (write-char ch out)
             (setf in-comment t))
            
            ;; Handle newlines
            ((char= ch #\Newline)
             (write-char ch out)
             (setf in-comment nil)
             (incf line)
             (setf column 0))
            
            ;; Handle parentheses only when not in string or comment
            ((and (not in-string) (not in-comment))
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
                (write-char ch out))))
            
            ;; Regular character
            (t
             (write-char ch out))))
    (let ((missing depth))
      (loop repeat missing do (write-char #\) out))
      (values (get-output-stream-string out)
              (nreverse removed)
              missing))))

(defun check-file (filename)
  "Check parenthesis balance in a file with detailed analysis"
  (format t "~%Checking ~A...~%" filename)
  (let ((content (read-file-as-string filename)))
    (multiple-value-bind (opens closes max-depth missing unmatched-closes)
        (analyze-parens-detailed content)
      (format t "  Opens:  ~D~%" opens)
      (format t "  Closes: ~D~%" closes)
      (format t "  Max depth: ~D~%" max-depth)
      (if (and (= opens closes) (null unmatched-closes))
          (format t "  Status: BALANCED~%")
          (progn
            (format t "  Status: UNBALANCED~%")
            (when (plusp missing)
              (format t "  Missing closing parens: ~D~%" missing))
            (when unmatched-closes
              (format t "  Extra closing parens at:~%")
              (dolist (pos unmatched-closes)
                (format t "    line ~D, column ~D~%" (car pos) (cdr pos))))))
      (and (= opens closes) (null unmatched-closes)))))

(defun fix-file (input output)
  "Fix parentheses in a file"
  (let* ((content (read-file-as-string input)))
    (multiple-value-bind (fixed removed appended)
        (auto-fix-content content)
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
  (format t "Comprehensive Parenthesis Tools~%")
  (format t "Usage:~%")
  (format t "  sbcl --script tools/paren-tools.lisp check <file>~%")
  (format t "  sbcl --script tools/paren-tools.lisp depth <file>~%")
  (format t "  sbcl --script tools/paren-tools.lisp fix <file> <output-file>~%")
  (format t "  sbcl --script tools/paren-tools.lisp fix <file> --in-place~%")
  (format t "  sbcl --script tools/paren-tools.lisp all~%")
  (format t "~%")
  (format t "Commands:~%")
  (format t "  check      - Check balance with detailed analysis~%")
  (format t "  depth      - Show running depth analysis~%")
  (format t "  fix        - Fix parentheses in file~%")
  (format t "  all        - Check all source files~%"))

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
         (if (check-file (first rest))
             (sb-ext:exit :code 0)
             (sb-ext:exit :code 1)))
        
        ((string= command "depth")
         (unless (= (length rest) 1)
           (show-usage)
           (sb-ext:exit :code 1))
         (let ((content (read-file-as-string (first rest))))
           (show-running-depth content)))
        
        ((string= command "fix")
         (unless (and rest (second rest))
           (show-usage)
           (sb-ext:exit :code 1))
         (let ((input (first rest))
               (target (second rest)))
           (cond
             ((string= target "--in-place")
              (fix-file input input))
             (t
              (fix-file input target)))))
        
        ((string= command "all")
         (let ((files '("src/server/core.lisp"
                        "src/server/commands.lisp"
                        "src/server/runtime.lisp"
                        "src/combat.lisp"
                        "src/inventory.lisp")))
           (dolist (file files)
             (check-file file))))
        
        (t
         (format t "Unknown command: ~A~%" command)
         (show-usage)
         (sb-ext:exit :code 1))))))

(main)
