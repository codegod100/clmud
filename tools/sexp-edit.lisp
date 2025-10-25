#!/usr/bin/env sbcl --script
;;; S-Expression Safe Editor
;;; Manipulates Lisp files at the s-expression level to maintain balance

(require :sb-bsd-sockets)

(defvar *packages-loaded* nil)

(defun ensure-packages-loaded ()
  "Load packages.lisp if not already loaded"
  (unless *packages-loaded*
    (handler-case
        (progn
          (load "src/packages.lisp")
          (setf *packages-loaded* t))
      (error (e)
        (format t "Warning: Could not load packages: ~A~%" e)))))

(defun load-file-as-string (filename)
  "Read entire file as a string"
  (with-open-file (stream filename :direction :input)
    (let ((contents (make-string (file-length stream))))
      (read-sequence contents stream)
      contents)))

(defun save-string-to-file (filename content)
  "Write string to file"
  (with-open-file (stream filename :direction :output :if-exists :supersede)
    (write-sequence content stream)))

(defun read-all-forms (filename)
  "Read all top-level forms from a file, returning list of (form . position)"
  (ensure-packages-loaded)
  (with-open-file (stream filename :direction :input)
    (loop for form = (handler-case (read stream nil 'eof)
                       (error (e)
                         (format t "ERROR reading form: ~A~%" e)
                         (return result)))
          for pos = (file-position stream)
          until (eq form 'eof)
          collect (cons form pos) into result
          finally (return result))))

(defun count-forms (filename)
  "Count top-level forms in a file"
  (length (read-all-forms filename)))

(defun check-balance (filename)
  "Check if parentheses are balanced"
  (handler-case
      (progn
        (read-all-forms filename)
        t)
    (end-of-file ()
      nil)))

(defun print-form-at-index (filename index)
  "Print the form at the given index (0-based)"
  (let ((forms (read-all-forms filename)))
    (if (< index (length forms))
        (let ((form (car (nth index forms))))
          (format t "Form ~D:~%" index)
          (pprint form)
          (terpri))
        (format t "ERROR: Index ~D out of range (0-~D)~%" index (1- (length forms))))))

(defun delete-form-at-index (filename index output-filename)
  "Delete the form at the given index and write to output file"
  (let ((forms (read-all-forms filename)))
    (if (< index (length forms))
        (progn
          (with-open-file (out output-filename :direction :output :if-exists :supersede)
            (with-open-file (in filename :direction :input)
              (loop for i from 0
                    for (form . pos) in forms
                    do (unless (= i index)
                         ;; Write the form with proper indentation
                         (pprint form out)
                         (terpri out)))))
          (format t "✓ Deleted form ~D from ~A -> ~A~%" index filename output-filename)
          t)
        (progn
          (format t "ERROR: Index ~D out of range (0-~D)~%" index (1- (length forms)))
          nil))))

(defun insert-form-before-index (filename form-string index output-filename)
  "Insert a new form before the given index"
  (let ((forms (read-all-forms filename))
        (new-form (handler-case (read-from-string form-string)
                    (error (e)
                      (format t "ERROR: Cannot read form: ~A~%" e)
                      (return-from insert-form-before-index nil)))))
    (if (<= index (length forms))
        (progn
          (with-open-file (out output-filename :direction :output :if-exists :supersede)
            (loop for i from 0
                  for (form . pos) in forms
                  do (when (= i index)
                       (pprint new-form out)
                       (terpri out))
                     (pprint form out)
                     (terpri out))
            ;; If inserting at end
            (when (= index (length forms))
              (pprint new-form out)
              (terpri out)))
          (format t "✓ Inserted form before index ~D: ~A -> ~A~%" index filename output-filename)
          t)
        (progn
          (format t "ERROR: Index ~D out of range (0-~D)~%" index (length forms))
          nil))))

(defun replace-form-at-index (filename form-string index output-filename)
  "Replace the form at the given index with a new form"
  (let ((forms (read-all-forms filename))
        (new-form (handler-case (read-from-string form-string)
                    (error (e)
                      (format t "ERROR: Cannot read form: ~A~%" e)
                      (return-from replace-form-at-index nil)))))
    (if (< index (length forms))
        (progn
          (with-open-file (out output-filename :direction :output :if-exists :supersede)
            (loop for i from 0
                  for (form . pos) in forms
                  do (if (= i index)
                         (pprint new-form out)
                         (pprint form out))
                     (terpri out)))
          (format t "✓ Replaced form ~D: ~A -> ~A~%" index filename output-filename)
          t)
        (progn
          (format t "ERROR: Index ~D out of range (0-~D)~%" index (1- (length forms)))
          nil))))

(defun show-usage ()
  (format t "S-Expression Safe Editor~%")
  (format t "Usage:~%")
  (format t "  sbcl --script sexp-edit.lisp list <file>~%")
  (format t "  sbcl --script sexp-edit.lisp count <file>~%")
  (format t "  sbcl --script sexp-edit.lisp check <file>~%")
  (format t "  sbcl --script sexp-edit.lisp show <file> <index>~%")
  (format t "  sbcl --script sexp-edit.lisp delete <file> <index> <output>~%")
  (format t "  sbcl --script sexp-edit.lisp insert <file> <index> <form-string> <output>~%")
  (format t "  sbcl --script sexp-edit.lisp replace <file> <index> <form-string> <output>~%")
  (format t "~%")
  (format t "Examples:~%")
  (format t "  sbcl --script sexp-edit.lisp list src/server.lisp~%")
  (format t "  sbcl --script sexp-edit.lisp count src/server.lisp~%")
  (format t "  sbcl --script sexp-edit.lisp show src/server.lisp 5~%")
  (format t "  sbcl --script sexp-edit.lisp delete src/server.lisp 10 /tmp/out.lisp~%")
  (format t "  sbcl --script sexp-edit.lisp replace src/server.lisp 3 '(defvar *test* 42)' /tmp/out.lisp~%"))

(defun main ()
  (let ((args sb-ext:*posix-argv*))
    ;; Skip the script name itself
    (setf args (rest args))

    (when (< (length args) 2)
      (show-usage)
      (sb-ext:exit :code 1))

    (let ((command (first args))
          (file (second args)))

      (cond
        ((string= command "list")
         (let ((forms (read-all-forms file)))
           (format t "~D top-level forms in ~A:~%" (length forms) file)
           (loop for (form . pos) in forms
                 for i from 0
                 do (format t "~3D: ~A~%" i (first form)))))

        ((string= command "count")
         (format t "~D~%" (count-forms file)))

        ((string= command "check")
         (if (check-balance file)
             (progn
               (format t "✓ ~A is balanced~%" file)
               (sb-ext:exit :code 0))
             (progn
               (format t "✗ ~A has unbalanced parens~%" file)
               (sb-ext:exit :code 1))))

        ((string= command "show")
         (when (< (length args) 3)
           (format t "ERROR: show requires index~%")
           (sb-ext:exit :code 1))
         (print-form-at-index file (parse-integer (third args))))

        ((string= command "delete")
         (when (< (length args) 4)
           (format t "ERROR: delete requires index and output file~%")
           (sb-ext:exit :code 1))
         (unless (delete-form-at-index file
                                       (parse-integer (third args))
                                       (fourth args))
           (sb-ext:exit :code 1)))

        ((string= command "insert")
         (when (< (length args) 5)
           (format t "ERROR: insert requires index, form, and output file~%")
           (sb-ext:exit :code 1))
         (unless (insert-form-before-index file
                                           (fourth args)
                                           (parse-integer (third args))
                                           (fifth args))
           (sb-ext:exit :code 1)))

        ((string= command "replace")
         (when (< (length args) 5)
           (format t "ERROR: replace requires index, form, and output file~%")
           (sb-ext:exit :code 1))
         (unless (replace-form-at-index file
                                        (fourth args)
                                        (parse-integer (third args))
                                        (fifth args))
           (sb-ext:exit :code 1)))

        (t
         (format t "ERROR: Unknown command '~A'~%" command)
         (show-usage)
         (sb-ext:exit :code 1))))))

(main)
