(let* ((script (or *load-truename*
                    (ignore-errors (truename (first sb-ext:*posix-argv*)))))
       (dir (and script (pathname-directory script))))
  (when dir
    (setf *default-pathname-defaults*
          (make-pathname :directory dir :defaults script))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  ;; Load SBCL contribs needed by the server before package definitions run.
  (require :sb-bsd-sockets))
  (require :asdf)

(load "src/packages.lisp")

(load "src/ansi.lisp")
(load "src/player.lisp")
(load "src/inventory.lisp")
(load "src/world.lisp")
(load "src/mob.lisp")
(load "src/combat.lisp")
(load "src/quest.lisp")
(load "src/server/core.lisp")
(load "src/server/commands.lisp")
(load "src/server/runtime.lisp")

(defun string-prefix-p (prefix string)
  (and (<= (length prefix) (length string))
       (= (search prefix string) 0)))

(defun maybe-parse-port (value)
  (let ((str (and value (string-trim '(#\Space #\Tab) value))))
    (when (and str (> (length str) 0))
      (ignore-errors
        (let ((number (parse-integer str :junk-allowed t)))
          (when (and number (> number 0) (< number 65536))
            number))))))

(defun detect-port-from-argv (argv)
  (loop for current on (rest argv)
        for arg = (car current)
        do
          (cond
            ((or (string= arg "--port") (string= arg "-p"))
             (let ((next (cadr current)))
               (return (maybe-parse-port next))))
            ((string-prefix-p "--port=" arg)
             (return (maybe-parse-port (subseq arg 7))))
            ((string-prefix-p "-p" arg)
             (when (> (length arg) 2)
               (return (maybe-parse-port (subseq arg 2))))))
        finally (return nil)))

(defun detect-port ()
  (or (detect-port-from-argv sb-ext:*posix-argv*)
      (maybe-parse-port (sb-ext:posix-getenv "MUD_PORT"))
      (maybe-parse-port (sb-ext:posix-getenv "PORT"))
      4000))

(defun main ()
  (let ((port (detect-port)))
    (handler-case
        (mud.server:start :port port)
      (error (err)
        (format *error-output* "~&Failed to start server on port ~a: ~a~%" port err)
        (finish-output *error-output*)
        (sb-ext:exit :code 1)))
  (format t "~&~a~%" (mud.ansi:wrap (format nil "MUD listening on port ~a. Press Ctrl+C to stop." port)
                    :bright-magenta))
    (finish-output)
    (handler-case
        (mud.server:await)
      (sb-sys:interactive-interrupt ()
        (format t "~&Interrupt received. Shutting down...~%")
        (finish-output)
        (mud.server:stop))
      (error (err)
        (format *error-output* "~&Fatal error: ~a~%" err)
        (finish-output *error-output*)
        (mud.server:stop)
        (sb-ext:exit :code 1)))
  (mud.server:stop)
  (format t "~&~a~%" (mud.ansi:wrap "Server stopped." :bright-black))
  (finish-output)
  (sb-ext:exit :code 0)))

(main)