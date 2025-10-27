;; Test settings command and mob corpse system
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
(load "../../src/server/runtime.lisp")

;; Initialize the world
(mud.world:initialize-world)

(format t "Testing settings command and mob corpse system...~%")

;; Create a test player
(let* ((test-stream (make-string-output-stream))
       (player (mud.player:make-player :name "TestPlayer" 
                                      :room :village-square 
                                      :stream test-stream 
                                      :socket nil)))
  
  ;; Test 1: Check initial settings
  (format t "~%Test 1: Initial settings~%")
  (mud.server::command-settings player "")
  (let ((output (get-output-stream-string test-stream)))
    (format t "Settings output: ~a~%" output)
    (assert (search "Auto-fight: OFF" output))
    (assert (search "Auto-loot: OFF" output)))
  
  ;; Test 2: Toggle auto-fight
  (format t "~%Test 2: Toggle auto-fight~%")
  (setf test-stream (make-string-output-stream))
  (setf (mud.player::player-stream player) test-stream)
  (mud.server::command-auto-fight player "")
  (let ((output (get-output-stream-string test-stream)))
    (format t "Auto-fight output: ~a~%" output)
    (assert (search "Auto-fight is now ON" output)))
  
  ;; Test 3: Toggle auto-loot
  (format t "~%Test 3: Toggle auto-loot~%")
  (setf test-stream (make-string-output-stream))
  (setf (mud.player::player-stream player) test-stream)
  (mud.server::command-auto-loot player "")
  (let ((output (get-output-stream-string test-stream)))
    (format t "Auto-loot output: ~a~%" output)
    (assert (search "Auto-loot is now ON" output)))
  
  ;; Test 4: Check updated settings
  (format t "~%Test 4: Updated settings~%")
  (setf test-stream (make-string-output-stream))
  (setf (mud.player::player-stream player) test-stream)
  (mud.server::command-settings player "")
  (let ((output (get-output-stream-string test-stream)))
    (format t "Updated settings output: ~a~%" output)
    (assert (search "Auto-fight: ON" output))
    (assert (search "Auto-loot: ON" output)))
  
  ;; Test 5: Test corpse creation
  (format t "~%Test 5: Corpse creation~%")
  (let* ((test-items (list (mud.inventory::create-item "rusty-dagger")
                          (mud.inventory::create-item "gold-coin")))
         (corpse (mud.world::create-corpse "test-goblin" test-items :village-square)))
    (assert (string= (mud.world::corpse-mob-name corpse) "test-goblin"))
    (assert (= (length (mud.world::corpse-items corpse)) 2))
    (assert (eq (mud.world::corpse-room-id corpse) :village-square))
    (format t "✓ Corpse created successfully~%"))
  
  ;; Test 6: Test corpse management
  (format t "~%Test 6: Corpse management~%")
  (let* ((test-items (list (mud.inventory::create-item "rusty-dagger")))
         (corpse (mud.world::create-corpse "test-wolf" test-items :village-square)))
    (mud.world::add-corpse-to-room :village-square corpse)
    (let ((corpses (mud.world::get-corpses-in-room :village-square)))
      (assert (= (length corpses) 1))
      (assert (eq (first corpses) corpse)))
    
    (let ((found-corpse (mud.world::find-corpse-in-room :village-square "wolf")))
      (assert (eq found-corpse corpse)))
    
    (let ((looted-items (mud.world::loot-corpse corpse)))
      (assert (= (length looted-items) 1))
      (assert (null (mud.world::corpse-items corpse))))
    
    (mud.world::remove-corpse-from-room :village-square corpse)
    (let ((corpses (mud.world::get-corpses-in-room :village-square)))
      (assert (null corpses)))
    (format t "✓ Corpse management works correctly~%"))
  
  (format t "~%All tests passed! ✓~%"))

(format t "~%Settings and corpse system test completed successfully!~%")
