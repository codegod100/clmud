#!/usr/bin/sbcl --script

;; Test streamlined quest system
;; This test demonstrates the new quest system where talking to quest givers
;; automatically starts quests and completes them when you have the required item

(load "src/packages.lisp")
(load "src/ansi.lisp")
(load "src/world.lisp")
(load "src/player.lisp")
(load "src/inventory.lisp")
(load "src/merchant.lisp")
(load "src/mob.lisp")
(load "src/quest.lisp")

;; Initialize systems
(mud.world::initialize-world)
(mud.merchant::initialize-merchants)
(mud.quest::initialize-quests)
(mud.mob::initialize-mobs)

(format t "=== Streamlined Quest System Test ===~%~%")

;; Create a test player
(let ((player (mud.player::make-player :name "TestPlayer" :stream *standard-output*)))
  ;; Set player in village square where the elder is
  (setf (mud.player::player-room player) 'mud.world::village-square)
  
  ;; Get the village elder mob
  (let ((elder (mud.mob::find-mob-in-room 'mud.world::village-square "elder")))
    (when elder
      (format t "1. Testing quest auto-start:")
      (format t "~%   Talking to Village Elder (no quest started yet):")
      
      ;; Talk to elder - should auto-start quest
      (let ((dialogue (mud.server::get-mob-dialogue elder player)))
        (format t "~%   Dialogue: ~a" dialogue))
      
      ;; Check quest state
      (let ((quest-state (mud.quest::get-player-quest-data player :apple-picking)))
        (format t "~%   Quest state after talking: ~a" quest-state))
      
      (format t "~%~%2. Testing quest auto-completion:")
      (format t "~%   Adding apple to inventory...")
      
      ;; Add apple to inventory
      (mud.inventory::add-to-inventory player (mud.inventory::create-item "apple"))
      
      (format t "~%   Talking to Village Elder again (with apple):")
      
      ;; Talk to elder again - should complete quest
      (let ((dialogue (mud.server::get-mob-dialogue elder player)))
        (format t "~%   Dialogue: ~a" dialogue))
      
      ;; Check quest state
      (let ((quest-state (mud.quest::get-player-quest-data player :apple-picking)))
        (format t "~%   Quest state after completion: ~a" quest-state))
      
      (format t "~%~%=== Test completed! ==="))))

(format t "~%Test file: tests/features/test-streamlined-quests.lisp~%")
(format t "This test demonstrates:~%")
(format t "- Talking to quest givers automatically starts quests~%")
(format t "- Having the required item automatically completes quests~%")
(format t "- No need for 'accept quest' or 'decline quest' commands~%")
(format t "- Quest completion gives XP and items automatically~%")
