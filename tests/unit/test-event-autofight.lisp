#!/usr/bin/env sbcl --script

;; Test event-based autofight system
;; This test verifies that autofight works correctly with the event system

(load "src/packages.lisp")
(load "src/ansi.lisp")
(load "src/world.lisp")
(load "src/player.lisp")
(load "src/mob.lisp")
(load "src/inventory.lisp")
(load "src/quest.lisp")
(load "src/combat.lisp")
(load "src/server/core.lisp")
(load "src/server/runtime.lisp")
(load "src/server/commands/core.lisp")
(load "src/server/commands/combat.lisp")

(in-package :mud.server)

(defun test-event-autofight ()
  "Test that event-based autofight works correctly"
  ;; Initialize world and mobs
  (mud.world::initialize-world)
  (mud.mob::initialize-mobs)
  
  (let* ((player (mud.player::make-player :name "TestPlayer" :room :village-square))
         (mob (mud.mob::spawn-mob :goblin :village-square)))
    
    ;; Set up the test environment
    (setf (mud.player::player-auto-fight player) t) ; Enable autofight
    (setf (mud.player::player-health player) 100) ; Give player enough health
    (setf (mud.mob::mob-health mob) 50) ; Give mob moderate health
    
    (format t "~%=== Testing Event-Based Autofight System ===")
    (format t "~%Player health: ~d" (mud.player::player-health player))
    (format t "~%Mob health: ~d" (mud.mob::mob-health mob))
    (format t "~%Player autofight: ~a" (mud.player::player-auto-fight player))
    (format t "~%Tick interval: ~d seconds" mud.constants::*tick-interval*)
    
    ;; Start combat manually
    (mud.mob::start-combat mob player)
    (format t "~%Combat started: ~a" (mud.mob::mob-in-combat-p mob))
    (format t "~%Player in autofight queue: ~a" (gethash player mud.mob::*autofight-players*))
    
    ;; Test initial state - no autofight yet
    (format t "~%~%Testing initial state...")
    (let ((*standard-output* (make-string-output-stream)))
      (mud.mob::process-autofight-actions))
    (format t "~%Mob health after initial autofight check: ~d" (mud.mob::mob-health mob))
    
    ;; Test autofight processing
    (format t "~%~%Testing autofight processing...")
    (let ((*standard-output* (make-string-output-stream)))
      (mud.mob::process-autofight-actions))
    (format t "~%Mob health after autofight: ~d" (mud.mob::mob-health mob))
    
    ;; Test the full combat processing
    (format t "~%~%Testing full combat processing...")
    (let ((*standard-output* (make-string-output-stream)))
      (mud.mob::process-all-mob-combat))
    (format t "~%Mob health after full combat processing: ~d" (mud.mob::mob-health mob))
    
    ;; Test multiple rounds
    (format t "~%~%Testing multiple rounds...")
    (dotimes (i 3)
      (format t "~%Round ~d:" (1+ i))
      (format t "~%  Player health: ~d" (mud.player::player-health player))
      (format t "~%  Mob health: ~d" (mud.mob::mob-health mob))
      
      ;; Process combat
      (let ((*standard-output* (make-string-output-stream)))
        (mud.mob::process-all-mob-combat))
      
      ;; Check if combat should continue
      (when (or (not (mud.player::player-alive-p player))
                (not (mud.mob::mob-alive-p mob)))
        (format t "~%  Combat ended - someone died!")
        (return)))
    
    ;; Check final state
    (let ((player-alive (mud.player::player-alive-p player))
          (mob-alive (mud.mob::mob-alive-p mob)))
      (format t "~%~%Final state:")
      (format t "~%Player alive: ~a" player-alive)
      (format t "~%Mob alive: ~a" mob-alive)
      (format t "~%Player health: ~d" (mud.player::player-health player))
      (format t "~%Mob health: ~d" (mud.mob::mob-health mob))
      (format t "~%Combat ended: ~a" (not (mud.mob::mob-in-combat-p mob)))
      (format t "~%Player in autofight queue: ~a" (gethash player mud.mob::*autofight-players*))
      
      ;; Verify that both parties took damage (autofight is working)
      (assert (< (mud.player::player-health player) 100) nil
              "Player should have taken damage")
      (assert (< (mud.mob::mob-health mob) 50) nil
              "Mob should have taken damage"))
    
    (format t "~%~%✅ Event-based autofight test passed!")
    t))

(defun run-test ()
  "Run the event autofight test"
  (handler-case
      (test-event-autofight)
    (error (e)
      (format t "~%~%❌ Test failed: ~a" e)
      (format t "~%Backtrace:")
      (sb-debug:print-backtrace)
      nil)))

;; Run the test
(run-test)
