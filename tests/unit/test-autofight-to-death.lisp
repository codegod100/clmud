#!/usr/bin/env sbcl --script

;; Test autofight to death functionality
;; This test verifies that when autofight is enabled and player attacks first,
;; the combat continues until one party dies.

(load "src/packages.lisp")
(load "src/ansi.lisp")
(load "src/world.lisp")
(load "src/player.lisp")
(load "src/mob.lisp")
(load "src/inventory.lisp")
(load "src/quest.lisp")
(load "src/server/core.lisp")
(load "src/server/commands/core.lisp")
(load "src/server/commands/combat.lisp")

(in-package :mud.server)

(defun test-autofight-to-death ()
  "Test that autofight continues fighting to the death when player attacks first"
  ;; Initialize world and mobs
  (mud.world::initialize-world)
  (mud.mob::initialize-mobs)
  
  (let* ((player (mud.player::make-player :name "TestPlayer" :room :village-square))
         (mob (mud.mob::spawn-mob :goblin :village-square)))
    
    ;; Set up the test environment
    (setf (mud.player::player-auto-fight player) t) ; Enable autofight
    (setf (mud.player::player-health player) 100) ; Give player enough health
    (setf (mud.mob::mob-health mob) 50) ; Give mob moderate health
    
    (format t "~%=== Testing Autofight to Death ===")
    (format t "~%Player health: ~d" (mud.player::player-health player))
    (format t "~%Mob health: ~d" (mud.mob::mob-health mob))
    (format t "~%Player autofight: ~a" (mud.player::player-auto-fight player))
    (format t "~%Mob name: ~a" (mud.mob::mob-name mob))
    
    ;; Test the core autofight functionality directly
    (format t "~%~%Testing autofight counter-attack directly...")
    
    ;; Start combat manually
    (mud.mob::start-combat mob player)
    (format t "~%Combat started: ~a" (mud.mob::mob-in-combat-p mob))
    
    ;; Test autofight counter-attack
    (let ((*standard-output* (make-string-output-stream)))
      (mud.server::auto-fight-counter-attack player mob)
      (format t "~%Autofight counter-attack executed"))
    
    ;; Check if mob took damage
    (format t "~%Mob health after autofight: ~d" (mud.mob::mob-health mob))
    
    ;; Test a few rounds of combat
    (dotimes (i 5)
      (when (and (mud.player::player-alive-p player)
                 (mud.mob::mob-alive-p mob))
        (format t "~%Round ~d:" (1+ i))
        (format t "~%  Player health: ~d" (mud.player::player-health player))
        (format t "~%  Mob health: ~d" (mud.mob::mob-health mob))
        
        ;; Process mob combat attack (this should trigger autofight counter-attack)
        (let ((*standard-output* (make-string-output-stream)))
          (mud.mob::process-mob-combat-attack mob))
        
        ;; Check if autofight is working
        (when (and (mud.player::player-alive-p player)
                   (mud.mob::mob-alive-p mob)
                   (mud.player::player-auto-fight player))
          (format t "~%  Autofight should continue...")
          ;; The autofight counter-attack should happen automatically
          ;; in the process-mob-combat-attack function
        )))
    
    ;; Check final state
    (let ((player-alive (mud.player::player-alive-p player))
          (mob-alive (mud.mob::mob-alive-p mob)))
      (format t "~%~%Final state:")
      (format t "~%Player alive: ~a" player-alive)
      (format t "~%Mob alive: ~a" mob-alive)
      (format t "~%Combat ended: ~a" (not (mud.mob::mob-in-combat-p mob)))
      
      ;; At least one should be dead
      (assert (or (not player-alive) (not mob-alive)) nil
              "At least one party should be dead after autofight"))
    
    (format t "~%~%✅ Autofight to death test passed!")
    t))

(defun run-test ()
  "Run the autofight to death test"
  (handler-case
      (test-autofight-to-death)
    (error (e)
      (format t "~%~%❌ Test failed: ~a" e)
      (format t "~%Backtrace:")
      (sb-debug:print-backtrace)
      nil)))

;; Run the test
(run-test)
