#!/usr/bin/env sbcl --script

;; Test to reproduce the apple quest bug where completion message shows
;; even when player hasn't started the quest

(load "src/packages.lisp")
(load "src/player.lisp")
(load "src/quest.lisp")
(load "src/inventory.lisp")
(load "src/world.lisp")
(load "src/mob.lisp")
(load "src/server/commands/player.lisp")

(format t "=== Testing Apple Quest Bug ===~%")

;; Initialize quests
(mud.quest::initialize-quests)

;; Create a test player
(let ((test-player (mud.player::make-player :name "TestPlayer" :stream *standard-output*)))
  
  (format t "~%1. Testing quest state for new player (should be :not-started)~%")
  (let ((quest-state (mud.quest::get-player-quest-data test-player :apple-picking)))
    (format t "Apple quest state: ~a~%" quest-state)
    (if (eq quest-state :not-started)
        (format t "✓ Quest state is correct: :not-started~%")
        (format t "✗ Quest state is wrong: ~a (expected :not-started)~%" quest-state)))
  
  (format t "~%2. Testing quest completion check (should not complete)~%")
  (multiple-value-bind (completed leveled-up quest)
      (mud.quest::check-quest-completion test-player :apple-picking)
    (format t "Quest completion result: completed=~a, leveled-up=~a~%" completed leveled-up)
    (if (not completed)
        (format t "✓ Quest completion check works correctly (no completion)~%")
        (format t "✗ Quest completion check failed (quest completed when it shouldn't)~%")))
  
  (format t "~%3. Testing quest state after failed completion check~%")
  (let ((quest-state (mud.quest::get-player-quest-data test-player :apple-picking)))
    (format t "Apple quest state after check: ~a~%" quest-state)
    (if (eq quest-state :not-started)
        (format t "✓ Quest state remains :not-started~%")
        (format t "✗ Quest state changed unexpectedly: ~a~%" quest-state)))
  
  (format t "~%4. Testing maybe-announce-quest-rewards (should do nothing)~%")
  (mud.quest::maybe-announce-quest-rewards test-player)
  (format t "✓ maybe-announce-quest-rewards completed (no output expected)~%")
  
  (format t "~%5. Testing quest state after maybe-announce-quest-rewards~%")
  (let ((quest-state (mud.quest::get-player-quest-data test-player :apple-picking)))
    (format t "Apple quest state after maybe-announce: ~a~%" quest-state)
    (if (eq quest-state :not-started)
        (format t "✓ Quest state remains :not-started~%")
        (format t "✗ Quest state changed unexpectedly: ~a~%" quest-state)))
  
  (format t "~%6. Testing NPC dialogue (should show quest offer, not completion)~%")
  ;; Create a mock Village Elder mob
  (let ((village-elder (mud.mob::make-mob :id :village-elder :name "Village Elder" :quest-giver :apple-picking)))
    (let ((dialogue (mud.server.commands.player::get-mob-dialogue village-elder test-player)))
      (format t "Village Elder dialogue: ~a~%" dialogue)
      (if (search "apple" dialogue)
          (format t "✓ Dialogue mentions apple (quest offer)~%")
          (format t "✗ Dialogue doesn't mention apple~%"))
      (if (search "eyes light up" dialogue)
          (format t "✗ BUG: Dialogue shows completion message when quest not started!~%")
          (format t "✓ Dialogue doesn't show completion message~%")))))

(format t "~%=== Apple Quest Bug Test Complete ===~%")
