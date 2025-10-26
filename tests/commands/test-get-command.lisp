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
(load "../src/server/commands.lisp")
(load "../src/server/runtime.lisp")

;; Initialize the world
(mud.world:initialize-world)

;; Create a test player
(let ((player (mud.player:make-player :name "TestPlayer" :room (mud.world:starting-room))))
  (format t "Created test player: ~a~%" (mud.player:player-name player))
  (format t "Player room: ~a~%" (mud.player:player-room player))
  
  ;; Check if get command is registered
  (format t "~%Checking command dispatch table for 'get' command...~%")
  (let ((get-handler (gethash "get" mud.server::*command-dispatch*)))
    (if get-handler
        (format t "✓ Get command is registered: ~a~%" get-handler)
        (format t "✗ Get command is NOT registered~%")))
  
  ;; Check if grab command is registered
  (let ((grab-handler (gethash "grab" mud.server::*command-dispatch*)))
    (if grab-handler
        (format t "✓ Grab command is registered: ~a~%" grab-handler)
        (format t "✗ Grab command is NOT registered~%")))
  
  ;; Test the get command with no arguments
  (format t "~%Testing get command with no arguments...~%")
  (mud.server::command-get player "")
  
  ;; Test the get command with a non-existent item
  (format t "~%Testing get command with non-existent item...~%")
  (mud.server::command-get player "sword")
  
  (format t "~%Test completed.~%"))
