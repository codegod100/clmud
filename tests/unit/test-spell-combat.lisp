#!/usr/bin/env sbcl --script

;; Test spell casting combat initiation
;; This test verifies that casting a spell at a mob properly starts combat and enables autofight

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

(defun test-spell-combat ()
  "Test that spell casting properly initiates combat and autofight"
  ;; Initialize world and mobs
  (mud.world::initialize-world)
  (mud.mob::initialize-mobs)
  
  (let* ((player (mud.player::make-player :name "TestPlayer" :room :village-square))
         (mob (mud.mob::spawn-mob :goblin :village-square)))
    
    ;; Set up the test environment
    (setf (mud.player::player-auto-fight player) t) ; Enable autofight
    (setf (mud.player::player-health player) 100) ; Give player enough health
    (setf (mud.player::player-mana player) 50) ; Give player enough mana
    (setf (mud.mob::mob-health mob) 50) ; Give mob moderate health
    
    (format t "~%=== Testing Spell Combat Initiation ===")
    (format t "~%Player health: ~d" (mud.player::player-health player))
    (format t "~%Player mana: ~d" (mud.player::player-mana player))
    (format t "~%Mob health: ~d" (mud.mob::mob-health mob))
    (format t "~%Player autofight: ~a" (mud.player::player-auto-fight player))
    (format t "~%Mob in combat before spell: ~a" (mud.mob::mob-in-combat-p mob))
    
    ;; Test spell casting
    (format t "~%~%Testing spell casting...")
    (let ((*standard-output* (make-string-output-stream)))
      (cast-spell-at-mob player mob (mud.combat::find-spell "fireball")))
    
    ;; Check if combat started
    (format t "~%Mob in combat after spell: ~a" (mud.mob::mob-in-combat-p mob))
    (format t "~%Mob combat target: ~a" (mud.mob::mob-combat-target mob))
    (format t "~%Mob health after spell: ~d" (mud.mob::mob-health mob))
    (format t "~%Player mana after spell: ~d" (mud.player::player-mana player))
    
    ;; Verify combat started
    (assert (mud.mob::mob-in-combat-p mob) nil "Combat should have started after spell casting")
    (assert (eq (mud.mob::mob-combat-target mob) player) nil "Player should be the combat target")
    
    ;; Test that autofight timing is set up correctly
    (format t "~%~%Testing autofight timing...")
    (let ((should-continue (mud.mob::should-autofight-continue mob)))
      (format t "~%Should autofight continue immediately: ~a" should-continue)
      (assert (not should-continue) nil "Autofight should not continue immediately"))
    
    ;; Simulate time passing to allow autofight
    (setf (mud.mob::mob-last-autofight-time mob) 
          (- (get-universal-time) (1+ *mob-movement-interval*)))
    
    ;; Now autofight should be allowed
    (let ((should-continue (mud.mob::should-autofight-continue mob)))
      (format t "~%Should autofight continue after time: ~a" should-continue)
      (assert should-continue nil "Autofight should continue after sufficient time"))
    
    ;; Test autofight processing
    (format t "~%~%Testing autofight processing...")
    (let ((*standard-output* (make-string-output-stream)))
      (mud.mob::process-autofight-actions mob))
    
    (format t "~%Mob health after autofight: ~d" (mud.mob::mob-health mob))
    
    (format t "~%~%✅ Spell combat test passed!")
    t))

(defun run-test ()
  "Run the spell combat test"
  (handler-case
      (test-spell-combat)
    (error (e)
      (format t "~%~%❌ Test failed: ~a" e)
      (format t "~%Backtrace:")
      (sb-debug:print-backtrace)
      nil)))

;; Run the test
(run-test)
