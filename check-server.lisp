(require :sb-bsd-sockets)
(handler-bind ((warning #'muffle-warning))
  (load "src/packages.lisp"))

(dolist (file '("src/server/core.lisp"
                "src/server/commands.lisp"
                "src/server/runtime.lisp"))
  (format t "~%Inspecting ~A...~%" file)
  (handler-case
      (with-open-file (in file)
        (let ((pkg (find-package :mud.server)))
          (let ((*package* pkg))
            (loop for form = (read in nil :eof)
                  for n from 1
                  while (not (eq form :eof))
                  do (when (and (consp form) (eq (car form) 'defun))
                       (format t "Read function: ~a~%" (second form)))
                  finally (format t "Successfully read ~d forms~%" n)))))
    (error (e)
      (format t "ERROR reading ~A: ~a~%" file e))))
