;; Test the new map command with ASCII-safe characters
(require :sb-bsd-sockets)
(require :asdf)

;; Load the MUD system
(load "../../src/packages.lisp")
(load "../../src/ansi.lisp")
(load "../../src/player.lisp")
(load "../../src/inventory.lisp")
(load "../../src/merchant.lisp")
(load "../../src/world.lisp")
(load "../../src/mob.lisp")
(load "../../src/combat.lisp")
(load "../../src/quest.lisp")
(load "../../src/server/core.lisp")
(load "../../src/server/commands.lisp")

;; Initialize the world
(mud.world:initialize-world)

;; Test the new map command
(let ((player (mud.player:make-player :name "TestPlayer" :room 'village-square)))
  (format t "=== Testing Map Command (ASCII-Safe) ===~%~%")
  
  (format t "Testing 'map' command (should show ASCII artistic map)...~%")
  (mud.server::command-map player "")
  
  (format t "~%Test completed successfully!~%"))


