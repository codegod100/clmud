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

(format t "Loading commands.lisp...~%")

(handler-case
    (progn
      (load "../src/server/commands.lisp")
      (format t "✓ commands.lisp loaded successfully~%"))
    (error (err)
      (format t "✗ Error loading commands.lisp: ~a~%" err)))

;; Check if get command was registered
(format t "Checking if get command was registered...~%")
(let ((handler (gethash "get" mud.server::*command-dispatch*)))
  (if handler
      (format t "✓ Get command is registered: ~a~%" handler)
      (format t "✗ Get command is NOT registered~%")))

;; Check if the function exists
(format t "Checking if command-get function exists...~%")
(if (fboundp 'mud.server::command-get)
    (format t "✓ command-get function exists~%")
    (format t "✗ command-get function does NOT exist~%"))

(format t "~%Test completed.~%")
