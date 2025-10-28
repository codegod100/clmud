;; Test the new 2-level adjacency map command
(require :sb-bsd-sockets)

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

;; Initialize the world
(mud.world:initialize-world)

;; Test the new map command
(format t "=== Testing New 2-Level Adjacency Map Command ===~%~%")

;; Test the adjacency map function directly
(format t "Testing adjacency map from Village Square...~%")
(format t "~a~%" (mud.world:generate-artistic-map 'village-square nil))

(format t "~%Testing adjacency map from Market Stalls...~%")
(format t "~a~%" (mud.world:generate-artistic-map 'market-stalls nil))

(format t "~%Testing adjacency map from Tavern...~%")
(format t "~a~%" (mud.world:generate-artistic-map 'bronze-badger-tavern nil))

(format t "~%Test completed successfully!~%")

