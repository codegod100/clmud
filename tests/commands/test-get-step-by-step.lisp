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

;; Define handle-get-all function first
(defun handle-get-all (player)
  (format t "handle-get-all called~%"))

;; Try to define the get command step by step
(format t "Step 1: Basic get command...~%")

(handler-case
    (progn
      (define-command (("get" "grab") command-get) (player rest)
        (let ((item-name (string-trim '(#\  #\Tab) rest)))
          (cond
            ((zerop (length item-name))
             (format t "Get what? Usage: get <item>~%"))
            ((string-equal item-name "all")
             (handle-get-all player))
            (t
             (format t "Getting item: ~a~%")))))
      (format t "✓ Basic get command defined successfully~%"))
    (error (err)
      (format t "✗ Error defining basic get command: ~a~%" err)))

;; Check if it was registered
(let ((handler (gethash "get" *command-dispatch*)))
  (if handler
      (format t "✓ Get command is registered: ~a~%" handler)
      (format t "✗ Get command is NOT registered~%")))

(format t "~%Step 2: Adding find-item-in-room call...~%")

;; Try to add the find-item-in-room call
(handler-case
    (progn
      (define-command (("get2" "grab2") command-get2) (player rest)
        (let ((item-name (string-trim '(#\  #\Tab) rest)))
          (cond
            ((zerop (length item-name))
             (format t "Get what? Usage: get <item>~%"))
            ((string-equal item-name "all")
             (handle-get-all player))
            (t
             (let ((item (find-item-in-room (player-room player) item-name)))
               (if item
                   (format t "Found item: ~a~%" (item-name item))
                   (format t "Item not found: ~a~%" item-name)))))))
      (format t "✓ Get command with find-item-in-room defined successfully~%"))
    (error (err)
      (format t "✗ Error defining get command with find-item-in-room: ~a~%" err)))

;; Check if it was registered
(let ((handler (gethash "get2" *command-dispatch*)))
  (if handler
      (format t "✓ Get2 command is registered: ~a~%" handler)
      (format t "✗ Get2 command is NOT registered~%")))

(format t "~%Test completed.~%")
