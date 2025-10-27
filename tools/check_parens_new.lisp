(defun count-parens (text)
  "Count parentheses in text. Returns (opens . closes)"
  (let ((opens 0)
        (closes 0))
    (loop for char across text do
          (case char
            (#\( (incf opens))
            (#\) (incf closes))))
    (cons opens closes)))

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
        (depth-changes '())
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
      (values opens closes max-depth missing (nreverse unmatched-closes) depth-changes))))

(defun check-file (filename)
  "Check parenthesis balance in a file with detailed analysis"
  (format t "~%Checking ~A...~%" filename)
  (with-open-file (stream filename :direction :input)
    (let ((content (make-string (file-length stream))))
      (read-sequence content stream)
      (multiple-value-bind (opens closes max-depth missing unmatched-closes depth-changes)
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
        (and (= opens closes) (null unmatched-closes))))))

(defun show-running-depth (filename)
  "Show running parenthesis depth through a file, similar to check-paren-balance.py"
  (format t "~%Running depth analysis for ~A...~%" filename)
  (with-open-file (stream filename :direction :input)
    (let ((content (make-string (file-length stream))))
      (read-sequence content stream)
      (let ((depth 0)
            (line 1)
            (column 0)
            (in-string nil)
            (in-comment nil)
            (escape-next nil))
        (loop for char across content do
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
        (format t "~%Final depth: ~D~%" depth)))))

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

(defun show-usage ()
  (format t "Enhanced Parenthesis Checker~%")
  (format t "Usage:~%")
  (format t "  sbcl --script tools/check_parens.lisp check <file>~%")
  (format t "  sbcl --script tools/check_parens.lisp depth <file>~%")
  (format t "  sbcl --script tools/check_parens.lisp all~%")
  (format t "~%")
  (format t "Commands:~%")
  (format t "  check  - Check balance with detailed analysis~%")
  (format t "  depth  - Show running depth analysis~%")
  (format t "  all    - Check all source files~%"))

(defun main ()
  (let ((args sb-ext:*posix-argv*))
    ;; Skip the script name itself
    (setf args (rest args))
    
    (cond
      ((null args)
       (show-usage)
       (sb-ext:exit :code 1))
      
      ((string= (first args) "check")
       (when (< (length args) 2)
         (format t "ERROR: check requires filename~%")
         (sb-ext:exit :code 1))
       (let ((filename (second args)))
         (if (check-file filename)
             (sb-ext:exit :code 0)
             (sb-ext:exit :code 1))))
      
      ((string= (first args) "depth")
       (when (< (length args) 2)
         (format t "ERROR: depth requires filename~%")
         (sb-ext:exit :code 1))
       (show-running-depth (second args)))
      
      ((string= (first args) "all")
       (let ((files '("src/server/core.lisp"
                      "src/server/commands.lisp"
                      "src/server/runtime.lisp"
                      "src/combat.lisp"
                      "src/inventory.lisp")))
         (dolist (file files)
           (check-file file)
           (read-all-forms file))))
      
      (t
       (format t "ERROR: Unknown command '~A'~%" (first args))
       (show-usage)
       (sb-ext:exit :code 1)))))

(main)
