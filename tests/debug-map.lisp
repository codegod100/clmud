(load "src/packages.lisp")
(load "src/ansi.lisp")
(load "src/player.lisp")
(load "src/inventory.lisp")
(load "src/merchant.lisp")
(load "src/world.lisp")
(load "src/final-map.lisp")
(load "src/mob.lisp")
(load "src/combat.lisp")
(load "src/quest.lisp")
(load "src/game-state.lisp")
(load "src/server/core.lisp")
(load "src/server/commands.lisp")

(mud.world:initialize-world)

(format t "=== Debug Map Function ===~%~%")

;; Test with a known room
(let ((room-id 'moonlit-square))
  (format t "Testing room: ~a~%" room-id)
  (let ((room (mud.world:find-room room-id)))
    (format t "Room found: ~a~%" room)
    (when room
      (format t "Room name: ~a~%" (mud.world:room-name room))
      (format t "Room exits: ~a~%" (mud.world:room-exits room))
      (format t "~%Map output:~%")
      (let ((map-output (mud.world:generate-artistic-map room-id)))
        (format t "~a~%" map-output)))))

(format t "~%Test completed!~%")
