#!/usr/bin/env sbcl --script

;; Load required packages first
(require :sb-bsd-sockets)
(require :asdf)

;; Load the MUD system
(load "src/packages.lisp")
(load "src/ansi.lisp")
(load "src/player.lisp")
(load "src/inventory.lisp")
(load "src/merchant.lisp")
(load "src/world.lisp")
(load "src/mob.lisp")
(load "src/combat.lisp")
(load "src/quest.lisp")
(load "src/server/core.lisp")
(load "src/server/commands.lisp")
(load "src/server/runtime.lisp")

;; Initialize the world
(mud.world:initialize-world)

(format t "Testing command registration...~%")

;; Check all registered commands
(format t "~%All registered commands:~%")
(maphash (lambda (key value) 
           (format t "  ~a -> ~a~%" key value))
         mud.server::*command-dispatch*)

;; Check specifically for get and grab
(format t "~%Checking for get command...~%")
(let ((get-handler (gethash "get" mud.server::*command-dispatch*)))
  (if get-handler
      (format t "✓ Get command is registered: ~a~%" get-handler)
      (format t "✗ Get command is NOT registered~%")))

(format t "~%Checking for grab command...~%")
(let ((grab-handler (gethash "grab" mud.server::*command-dispatch*)))
  (if grab-handler
      (format t "✓ Grab command is registered: ~a~%" grab-handler)
      (format t "✗ Grab command is NOT registered~%")))

;; Test if the command-get function exists
(format t "~%Checking if command-get function exists...~%")
(if (fboundp 'mud.server::command-get)
    (format t "✓ command-get function exists~%")
    (format t "✗ command-get function does NOT exist~%"))

(format t "~%Test completed.~%")
