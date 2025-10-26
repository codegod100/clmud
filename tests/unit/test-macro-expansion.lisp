;; Load required packages first
(require :sb-bsd-sockets)
(require :asdf)

;; Load the MUD system
(load "../src/packages.lisp")
(load "../src/ansi.lisp")
(load "../src/player.lisp")
(load "../src/inventory.lisp")
(load "../src/merchant.lisp")
(load "../src/world.lisp")
(load "../src/mob.lisp")
(load "../src/combat.lisp")
(load "../src/quest.lisp")
(load "../src/server/core.lisp")

;; Test the macro expansion
(format t "Testing define-command macro expansion...~%")

;; Define a simple test command
(defmacro define-command ((names function-name) (player rest) &body body)
  `(progn
     (defun ,function-name (,player ,rest)
       ,@body)
     (register-command-handler ',names #',function-name)))

(defparameter *command-dispatch* (make-hash-table :test #'equal))

(defun register-command-handler (names handler)
  (dolist (name names)
    (setf (gethash (string-downcase name) *command-dispatch*) handler)))

;; Try to define a simple test command
(format t "Defining test command...~%")
(define-command (("test") command-test) (player rest)
  (format t "Test command called with: ~a~%" rest))

;; Check if it was registered
(format t "Checking if test command was registered...~%")
(let ((handler (gethash "test" *command-dispatch*)))
  (if handler
      (format t "✓ Test command is registered: ~a~%" handler)
      (format t "✗ Test command is NOT registered~%")))

;; Check if the function exists
(format t "Checking if command-test function exists...~%")
(if (fboundp 'command-test)
    (format t "✓ command-test function exists~%")
    (format t "✗ command-test function does NOT exist~%"))

(format t "~%Test completed.~%")
