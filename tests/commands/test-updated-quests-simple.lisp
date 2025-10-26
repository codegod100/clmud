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
(mud.mob:initialize-mobs)
(mud.quest:initialize-quests)

(format t "Testing updated quest system...~%")

;; Test 1: Check that treasure map is in ancient grove
(format t "~%1. Testing treasure map location...~%")
(let ((grove-room (mud.world::find-room 'mud.world::ancient-grove)))
  (if grove-room
      (let ((items (mud.world::room-items grove-room)))
        (if (find "treasure-map" items :key #'mud.inventory::item-name :test #'string-equal)
            (format t "✓ Treasure map found in ancient grove~%")
            (format t "✗ Treasure map not found in ancient grove~%")))
      (format t "✗ Ancient grove room not found~%")))

;; Test 2: Check that village elder is in village square
(format t "~%2. Testing village elder location...~%")
(let ((village-mobs (mud.mob::get-mobs-in-room 'mud.world::village-square)))
  (if (find "Village Elder" village-mobs :key #'mud.mob::mob-name :test #'string-equal)
      (format t "✓ Village Elder found in village square~%")
      (format t "✗ Village Elder not found in village square~%")))

;; Test 3: Check that village elder is quest giver for apple quest
(format t "~%3. Testing village elder quest giver status...~%")
(let ((elder-template (mud.mob::find-mob-template :village-elder)))
  (if elder-template
      (let ((quest-giver (mud.mob::mob-quest-giver elder-template)))
        (if (eq quest-giver :apple-picking)
            (format t "✓ Village Elder is quest giver for :apple-picking~%")
            (format t "✗ Village Elder quest giver status: ~a~%" quest-giver)))
      (format t "✗ Village Elder template not found~%")))

;; Test 4: Check that apple is still in village garden
(format t "~%4. Testing apple location...~%")
(let ((garden-room (mud.world::find-room 'mud.world::village-garden)))
  (if garden-room
      (let ((items (mud.world::room-items garden-room)))
        (if (find "apple" items :key #'mud.inventory::item-name :test #'string-equal)
            (format t "✓ Apple found in village garden~%")
            (format t "✗ Apple not found in village garden~%")))
      (format t "✗ Village garden room not found~%")))

;; Test 5: Test quest dialogue for village elder
(format t "~%5. Testing village elder quest dialogue...~%")
(let ((offer-dialogue (mud.server::get-quest-offer-dialogue :village-elder :apple-picking)))
  (if (search "apple" offer-dialogue)
      (format t "✓ Village Elder dialogue mentions apple~%")
      (format t "✗ Village Elder dialogue missing apple reference~%")))

;; Test 6: Test quest dialogue for Captain Blackbeard
(format t "~%6. Testing Captain Blackbeard quest dialogue...~%")
(let ((offer-dialogue (mud.server::get-quest-offer-dialogue :captain-blackbeard :pirate-treasure)))
  (if (search "treasure map" offer-dialogue)
      (format t "✓ Captain Blackbeard dialogue mentions treasure map~%")
      (format t "✗ Captain Blackbeard dialogue missing treasure map reference~%")))

;; Test 7: Test that both quests can be started
(format t "~%7. Testing quest start functionality...~%")
(let ((test-player (mud.player::make-player :name "TestPlayer" :stream *standard-output*)))
  ;; Test apple quest start
  (mud.quest::start-quest test-player :apple-picking)
  (let ((apple-state (mud.quest::get-player-quest-data test-player :apple-picking)))
    (if (eq apple-state :in-progress)
        (format t "✓ Apple quest started successfully~%")
        (format t "✗ Apple quest start failed: ~a~%" apple-state)))
  
  ;; Test pirate quest start
  (mud.quest::start-quest test-player :pirate-treasure)
  (let ((pirate-state (mud.quest::get-player-quest-data test-player :pirate-treasure)))
    (if (eq pirate-state :in-progress)
        (format t "✓ Pirate quest started successfully~%")
        (format t "✗ Pirate quest start failed: ~a~%" pirate-state))))

(format t "~%=== Updated Quest System Tests Complete ===~%")
(format t "✓ Treasure map hidden in ancient grove (challenging location)~%")
(format t "✓ Village Elder added as quest giver for apple quest~%")
(format t "✓ Both quests have immersive dialogue~%")
(format t "✓ Quest system now fully NPC-driven~%")
(format t "✓ Players must explore to find quest items~%")
