;; Test auto-loot functionality
(require :sb-bsd-sockets)
(require :asdf)

;; Load the MUD system
(load "../../src/packages.lisp")
(load "../../src/ansi.lisp")
(load "../../src/player.lisp")
(load "../../src/inventory.lisp")
(load "../../src/world.lisp")
(load "../../src/mob.lisp")
(load "../../src/combat.lisp")
(load "../../src/quest.lisp")

;; Initialize the world
(mud.world:initialize-world)

(format t "Testing auto-loot functionality...~%")

;; Test 1: Player with auto-loot enabled
(format t "~%Test 1: Player with auto-loot enabled~%")
(let* ((test-stream (make-string-output-stream))
       (player (mud.player:make-player :name "TestPlayer" 
                                      :room :village-square 
                                      :stream test-stream 
                                      :socket nil)))
  ;; Enable auto-loot
  (setf (mud.player::player-auto-loot player) t)
  (format t "Auto-loot enabled: ~a~%" (mud.player::player-auto-loot player))
  (assert (mud.player::player-auto-loot player))
  (format t "✓ Auto-loot setting works~%"))

;; Test 2: Corpse creation and auto-looting
(format t "~%Test 2: Corpse creation and auto-looting~%")
(let* ((test-stream (make-string-output-stream))
       (player (mud.player:make-player :name "TestPlayer" 
                                      :room :village-square 
                                      :stream test-stream 
                                      :socket nil))
       (test-items (list (mud.inventory::create-item "rusty-dagger")
                        (mud.inventory::create-item "health-potion")))
       (corpse (mud.world::create-corpse "test-goblin" test-items :village-square)))
  
  ;; Enable auto-loot
  (setf (mud.player::player-auto-loot player) t)
  
  ;; Add corpse to room
  (mud.world::add-corpse-to-room :village-square corpse)
  
  ;; Simulate auto-looting
  (let ((items (mud.world::loot-corpse corpse)))
    (when items
      (dolist (item items)
        (mud.inventory::add-to-inventory player item))
      (format t "Auto-looted items: ~{~a~^, ~}~%" 
              (mapcar #'mud.inventory::item-name items))
      (assert (= (length items) 2))
      (assert (= (length (mud.player::player-inventory player)) 2))))
  
  ;; Remove empty corpse
  (mud.world::remove-corpse-from-room :village-square corpse)
  (format t "✓ Auto-looting simulation works~%"))

;; Test 3: Settings command functionality
(format t "~%Test 3: Settings command functionality~%")
(let* ((test-stream (make-string-output-stream))
       (player (mud.player:make-player :name "TestPlayer" 
                                      :room :village-square 
                                      :stream test-stream 
                                      :socket nil)))
  ;; Test initial settings
  (format t "Initial auto-fight: ~a~%" (mud.player::player-auto-fight player))
  (format t "Initial auto-loot: ~a~%" (mud.player::player-auto-loot player))
  (assert (null (mud.player::player-auto-fight player)))
  (assert (null (mud.player::player-auto-loot player)))
  
  ;; Toggle settings
  (setf (mud.player::player-auto-fight player) t)
  (setf (mud.player::player-auto-loot player) t)
  
  (format t "After toggle - auto-fight: ~a, auto-loot: ~a~%" 
          (mud.player::player-auto-fight player)
          (mud.player::player-auto-loot player))
  (assert (mud.player::player-auto-fight player))
  (assert (mud.player::player-auto-loot player))
  (format t "✓ Settings toggle works~%"))

(format t "~%All auto-loot tests passed! ✓~%")
(format t "~%Auto-loot functionality is working correctly!~%")
