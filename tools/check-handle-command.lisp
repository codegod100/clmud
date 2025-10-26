(require :sb-bsd-sockets)
(load "src/packages.lisp")

;; Read forms 1-46
(with-open-file (s "src/server/commands.lisp")
  (dotimes (i 46)
    (read s)))

;; Now try to read handle-command and see how deep we are
(with-open-file (s "src/server/commands.lisp")
  ;; Skip to position 29869
  (file-position s 29869)
  (handler-case
      (let ((form (read s)))
        (format t "Successfully read handle-command!~%")
        (format t "Form type: ~A~%" (first form)))
    (end-of-file (e)
      (format t "END-OF-FILE while reading handle-command~%")
      (format t "This means handle-command is missing closing parens~%")
      ;; Count how many opens vs closes
      (file-position s 29869)
      (let ((depth 0)
            (max-depth 0))
        (handler-case
            (loop
             (let ((ch (read-char s)))
               (case ch
                 (#\( (incf depth) (setf max-depth (max max-depth depth)))
                 (#\) (decf depth)))))
          (end-of-file ()
            (format t "At EOF: depth=~D (needs ~D more closing parens)~%" depth depth)
            (format t "Max depth reached: ~D~%" max-depth)))))))
