;; Test quest reset functionality

(load "src/packages.lisp")
(load "src/player.lisp")
(load "src/quest.lisp")
(load "src/inventory.lisp")

(format t "=== Testing Quest Reset System ===~%")

;; Initialize quests
(mud.quest::initialize-quests)

;; Test 1: Check that quests are marked as repeatable
(format t "~%1. Testing quest repeatability...~%")
(let ((apple-quest (mud.quest::find-quest :apple-picking))
      (pirate-quest (mud.quest::find-quest :pirate-treasure)))
  (if (mud.quest::quest-repeatable apple-quest)
      (format t "✓ Apple quest is repeatable~%")
      (format t "✗ Apple quest is NOT repeatable~%"))
  (if (mud.quest::quest-repeatable pirate-quest)
      (format t "✓ Pirate quest is repeatable~%")
      (format t "✗ Pirate quest is NOT repeatable~%")))

;; Test 2: Test quest reset functionality
(format t "~%2. Testing quest reset...~%")
(let ((test-player (mud.player::make-player :name "TestPlayer" :stream *standard-output*)))
  
  ;; Start and complete apple quest
  (format t "   Starting apple quest...~%")
  (mud.quest::start-quest test-player :apple-picking)
  (format t "   Quest state: ~a~%" (mud.quest::get-player-quest-data test-player :apple-picking))
  
  ;; Simulate quest completion
  (setf (mud.player::player-quest-state test-player) (make-hash-table :test #'eq))
  (setf (gethash :apple-picking (mud.player::player-quest-state test-player)) :completed)
  (format t "   Quest completed, state: ~a~%" (mud.quest::get-player-quest-data test-player :apple-picking))
  
  ;; Try to reset the quest
  (format t "   Resetting quest...~%")
  (let ((result (mud.quest::reset-quest test-player :apple-picking)))
    (format t "   Reset result: ~a~%" result)
    (format t "   Quest state after reset: ~a~%" (mud.quest::get-player-quest-data test-player :apple-picking))
    
    (if (eq (mud.quest::get-player-quest-data test-player :apple-picking) :not-started)
        (format t "✓ Quest reset successful~%")
        (format t "✗ Quest reset failed~%"))))

;; Test 3: Test resetting non-repeatable quest (should fail)
(format t "~%3. Testing reset of non-repeatable quest...~%")
(let ((test-player (mud.player::make-player :name "TestPlayer2" :stream *standard-output*)))
  ;; Create a non-repeatable quest for testing
  (mud.quest::define-quest :test-quest
                           "Test Quest"
                           "A test quest that is not repeatable"
                           (lambda (player) nil)
                           100
                           "Test completion message"
                           :repeatable nil)
  
  (let ((result (mud.quest::reset-quest test-player :test-quest)))
    (format t "   Reset result: ~a~%" result)
    (if (search "not repeatable" result)
        (format t "✓ Non-repeatable quest correctly rejected reset~%")
        (format t "✗ Non-repeatable quest incorrectly allowed reset~%"))))

;; Test 4: Test starting a repeatable quest after completion
(format t "~%4. Testing starting repeatable quest after completion...~%")
(let ((test-player (mud.player::make-player :name "TestPlayer3" :stream *standard-output*)))
  ;; Set quest as completed
  (setf (mud.player::player-quest-state test-player) (make-hash-table :test #'eq))
  (setf (gethash :apple-picking (mud.player::player-quest-state test-player)) :completed)
  
  ;; Try to start the quest again
  (let ((result (mud.quest::start-quest test-player :apple-picking)))
    (format t "   Start result: ~a~%" result)
    (if (search "Quest started" result)
        (format t "✓ Repeatable quest can be started after completion~%")
        (format t "✗ Repeatable quest cannot be started after completion~%"))))

(format t "~%=== Quest Reset System Test Complete ===~%")
