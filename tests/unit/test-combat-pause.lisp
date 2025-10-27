#!/usr/bin/env sbcl --script

;; Test combat pause functionality
;; This test verifies that autofight respects pause intervals and doesn't immediately counter-attack

(load "src/packages.lisp")
(load "src/ansi.lisp")
(load "src/world.lisp")
(load "src/player.lisp")
(load "src/mob.lisp")
(load "src/inventory.lisp")
(load "src/quest.lisp")
(load "src/server/core.lisp")
(load "src/server/runtime.lisp")
(load "src/server/commands/core.lisp")
(load "src/server/commands/combat.lisp")

(in-package :mud.server)

(defun test-combat-pause ()
  "Test that combat pause system works correctly"
  ;; Initialize world and mobs
  (mud.world::initialize-world)
  (mud.mob::initialize-mobs)
  
  (let* ((player (mud.player::make-player :name "TestPlayer" :room :village-square))
         (mob (mud.mob::spawn-mob :goblin :village-square)))
    
    ;; Set up the test environment
    (setf (mud.player::player-auto-fight player) t) ; Enable autofight
    (setf (mud.player::player-health player) 100) ; Give player enough health
    (setf (mud.mob::mob-health mob) 50) ; Give mob moderate health
    
    (format t "~%=== Testing Combat Pause System ===")
    (format t "~%Player health: ~d" (mud.player::player-health player))
    (format t "~%Mob health: ~d" (mud.mob::mob-health mob))
    (format t "~%Player autofight: ~a" (mud.player::player-auto-fight player))
    (format t "~%Tick interval: ~d seconds" *mob-movement-interval*)
    
    ;; Start combat manually
    (mud.mob::start-combat mob player)
    (format t "~%Combat started: ~a" (mud.mob::mob-in-combat-p mob))
    
    ;; Test that autofight doesn't immediately trigger
    (format t "~%~%Testing immediate autofight check...")
    (let ((should-continue (mud.mob::should-autofight-continue mob)))
      (format t "~%Should autofight continue immediately: ~a" should-continue)
      (assert (not should-continue) nil "Autofight should not continue immediately"))
    
    ;; Test autofight counter-attack directly (should work)
    (format t "~%~%Testing direct autofight counter-attack...")
    (let ((*standard-output* (make-string-output-stream)))
      (mud.server::auto-fight-counter-attack player mob)
      (format t "~%Direct autofight executed"))
    
    ;; Check if mob took damage
    (format t "~%Mob health after direct autofight: ~d" (mud.mob::mob-health mob))
    
    ;; Update the autofight time to simulate time passing
    (setf (mud.mob::mob-last-autofight-time mob) (get-universal-time))
    
    ;; Test that autofight still doesn't trigger immediately after update
    (format t "~%~%Testing autofight check after time update...")
    (let ((should-continue (mud.mob::should-autofight-continue mob)))
      (format t "~%Should autofight continue after time update: ~a" should-continue)
      (assert (not should-continue) nil "Autofight should not continue immediately after time update"))
    
    ;; Simulate time passing by setting the last autofight time to past
    (setf (mud.mob::mob-last-autofight-time mob) 
          (- (get-universal-time) (1+ *mob-movement-interval*)))
    
    ;; Now autofight should be allowed
    (format t "~%~%Testing autofight check after sufficient time...")
    (let ((should-continue (mud.mob::should-autofight-continue mob)))
      (format t "~%Should autofight continue after sufficient time: ~a" should-continue)
      (assert should-continue nil "Autofight should continue after sufficient time"))
    
    ;; Test the autofight processing function
    (format t "~%~%Testing autofight processing function...")
    (let ((*standard-output* (make-string-output-stream)))
      (mud.mob::process-autofight-actions mob)
      (format t "~%Autofight processing executed"))
    
    ;; Check final state
    (let ((player-alive (mud.player::player-alive-p player))
          (mob-alive (mud.mob::mob-alive-p mob)))
      (format t "~%~%Final state:")
      (format t "~%Player alive: ~a" player-alive)
      (format t "~%Mob alive: ~a" mob-alive)
      (format t "~%Mob health: ~d" (mud.mob::mob-health mob))
      (format t "~%Combat ended: ~a" (not (mud.mob::mob-in-combat-p mob)))
      
      ;; The test passed - we verified the pause system works
      (format t "~%✅ Combat pause system is working correctly!"))
    
    (format t "~%~%✅ Combat pause test passed!")
    t))

(defun run-test ()
  "Run the combat pause test"
  (handler-case
      (test-combat-pause)
    (error (e)
      (format t "~%~%❌ Test failed: ~a" e)
      (format t "~%Backtrace:")
      (sb-debug:print-backtrace)
      nil)))

;; Run the test
(run-test)
