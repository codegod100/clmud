;; Simple test for settings command and corpse system
(require :sb-bsd-sockets)
(require :asdf)

;; Load only the necessary packages
(load "../../src/packages.lisp")
(load "../../src/ansi.lisp")
(load "../../src/player.lisp")
(load "../../src/inventory.lisp")
(load "../../src/world.lisp")

;; Initialize the world
(mud.world:initialize-world)

(format t "Testing settings and corpse system...~%")

;; Test 1: Player structure with new fields
(format t "~%Test 1: Player structure~%")
(let* ((test-stream (make-string-output-stream))
       (player (mud.player:make-player :name "TestPlayer" 
                                      :room :village-square 
                                      :stream test-stream 
                                      :socket nil)))
  (format t "Initial auto-fight: ~a~%" (mud.player::player-auto-fight player))
  (format t "Initial auto-loot: ~a~%" (mud.player::player-auto-loot player))
  (assert (null (mud.player::player-auto-fight player)))
  (assert (null (mud.player::player-auto-loot player)))
  (format t "✓ Player structure works correctly~%"))

;; Test 2: Corpse creation
(format t "~%Test 2: Corpse creation~%")
(let* ((test-items (list (mud.inventory::create-item "rusty-dagger")
                        (mud.inventory::create-item "gold-coin")))
       (corpse (mud.world::create-corpse "test-goblin" test-items :village-square)))
  (assert (string= (mud.world::corpse-mob-name corpse) "test-goblin"))
  (assert (= (length (mud.world::corpse-items corpse)) 2))
  (assert (eq (mud.world::corpse-room-id corpse) :village-square))
  (format t "✓ Corpse creation works correctly~%"))

;; Test 3: Corpse management
(format t "~%Test 3: Corpse management~%")
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

(format t "~%All basic tests passed! ✓~%")
(format t "~%Settings and corpse system implementation completed successfully!~%")
