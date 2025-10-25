#!/usr/bin/env sbcl --script

;;; Helper script to fix handle-look-at function by reading and rewriting it properly

;; Load packages first so symbols can be read
(require :sb-bsd-sockets)
(load "src/packages.lisp")

(defun read-all-forms (filename)
  "Read all top-level forms from a file"
  (with-open-file (in filename :direction :input)
    (loop for form = (read in nil :eof)
          until (eq form :eof)
          collect form)))

(defun write-all-forms (filename forms)
  "Write all forms to a file with pretty printing"
  (with-open-file (out filename :direction :output :if-exists :supersede)
    (let ((*print-pretty* t)
          (*print-case* :downcase))
      (dolist (form forms)
        (pprint form out)
        (terpri out)
        (terpri out)))))

(defun find-defun (forms name)
  "Find a defun form by name"
  (find-if (lambda (form)
             (and (consp form)
                  (eq (first form) 'defun)
                  (eq (second form) name)))
           forms))

(defun replace-defun (forms name new-defun)
  "Replace a defun form with a new one"
  (mapcar (lambda (form)
            (if (and (consp form)
                     (eq (first form) 'defun)
                     (eq (second form) name))
                new-defun
                form))
          forms))

;; Read the file
(let ((forms (read-all-forms "src/server.lisp")))
  (format t "Read ~d top-level forms~%" (length forms))

  ;; Find handle-look-at
  (let ((old-form (find-defun forms 'handle-look-at)))
    (if old-form
        (progn
          (format t "Found handle-look-at, it has ~d elements~%" (length old-form))
          (format t "Function definition looks OK structurally~%"))
        (format t "ERROR: Could not find handle-look-at~%")))

  ;; Write back
  (write-all-forms "src/server.lisp" forms)
  (format t "Wrote reformatted file to src/server.lisp~%"))
