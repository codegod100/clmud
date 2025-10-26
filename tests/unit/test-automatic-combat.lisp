#!/usr/bin/env sbcl --script

;; Test automatic combat system (without server dependencies)
(load "src/packages.lisp")
(load "src/world.lisp")
(load "src/player.lisp")
(load "src/mob.lisp")
(load "src/combat.lisp")

(defun test-combat-system ()
  "Test the automatic combat system"
  (format t "Testing automatic combat system...~%")
  
  ;; Initialize mobs
  (mud.mob::initialize-mobs)
  
  ;; Create a test player
  (let ((test-player (mud.player::make-player 
                      :name "TestPlayer"
                      :stream *standard-output*
                      :room 'mud.world::village-square)))
    
    ;; Spawn a test mob
    (let ((test-mob (mud.mob::spawn-mob 'mud.mob::goblin 'mud.world::village-square)))
      (when test-mob
        (format t "Spawned test mob: ~a~%" (mud.mob::mob-name test-mob))
        
        ;; Test starting combat
        (mud.mob::start-combat test-mob test-player)
        (format t "Started combat between ~a and ~a~%" 
                (mud.mob::mob-name test-mob) 
                (mud.player::player-name test-player))
        
        ;; Check combat state
        (format t "Mob in combat: ~a~%" (mud.mob::mob-in-combat-p test-mob))
        (format t "Combat target: ~a~%" (mud.mob::mob-combat-target test-mob))
        
        ;; Test ending combat
        (mud.mob::end-combat test-mob)
        (format t "Ended combat. Mob in combat: ~a~%" (mud.mob::mob-in-combat-p test-mob))
        
        ;; Test combat attack conditions
        (mud.mob::start-combat test-mob test-player)
        (format t "Should mob attack: ~a~%" (mud.mob::should-mob-attack-in-combat test-mob))
        
        (format t "Automatic combat system test completed successfully!~%")
        t))))

;; Run the test
(handler-case
    (test-combat-system)
  (error (err)
    (format t "Test failed with error: ~a~%" err)
    (sb-ext:exit :code 1)))
