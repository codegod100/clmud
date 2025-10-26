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

;; Initialize the world
(mud.world:initialize-world)

;; Set up the command system
(defparameter *command-dispatch* (make-hash-table :test #'equal))

(defun register-command-handler (names handler)
  (dolist (name names)
    (setf (gethash (string-downcase name) *command-dispatch*) handler)))

(defmacro define-command ((names function-name) (player rest) &body body)
  `(progn
     (defun ,function-name (,player ,rest)
       ,@body)
     (register-command-handler ',names #',function-name)))

;; Try to define the get command
(format t "Attempting to define get command...~%")

(handler-case
    (progn
      (define-command (("get" "grab") command-get) (player rest)
        (let ((item-name (string-trim '(#\  #\Tab) rest)))
          (cond
            ((zerop (length item-name))
             (format t "Get what? Usage: get <item>~%"))
            (t
             (format t "Getting item: ~a~%" item-name)))))
      (format t "✓ Get command defined successfully~%"))
    (error (err)
      (format t "✗ Error defining get command: ~a~%" err)))

;; Check if it was registered
(format t "Checking if get command was registered...~%")
(let ((handler (gethash "get" *command-dispatch*)))
  (if handler
      (format t "✓ Get command is registered: ~a~%" handler)
      (format t "✗ Get command is NOT registered~%")))

;; Check if the function exists
(format t "Checking if command-get function exists...~%")
(if (fboundp 'command-get)
    (format t "✓ command-get function exists~%")
    (format t "✗ command-get function does NOT exist~%"))

(format t "~%Test completed.~%")
