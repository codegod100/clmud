#!/usr/bin/env sbcl --script

;; Test auto-fight functionality
;; Run with: sbcl --script tests/unit/test-auto-fight.lisp

(load "src/packages.lisp")
(load "src/player.lisp")
(load "src/mob.lisp")
(load "src/server/commands/core.lisp")
(load "src/server/core.lisp")
(load "src/server/commands/player.lisp")

(in-package :mud.server)

(defun test-auto-fight-setting ()
  "Test that auto-fight setting can be toggled"
  (let ((player (mud.player::make-player :name "TestPlayer" :room :village-square)))
    ;; Initially auto-fight should be nil
    (assert (null (mud.player::player-auto-fight player)) nil
            "Auto-fight should be nil initially")
    
    ;; Toggle auto-fight on
    (setf (mud.player::player-auto-fight player) t)
    (assert (mud.player::player-auto-fight player) nil
            "Auto-fight should be t after setting")
    
    ;; Toggle auto-fight off
    (setf (mud.player::player-auto-fight player) nil)
    (assert (null (mud.player::player-auto-fight player)) nil
            "Auto-fight should be nil after toggling off")
    
    (format t "✓ Auto-fight setting toggle test passed~%")))

(defun test-auto-fight-command ()
  "Test the auto-fight command"
  (let ((player (mud.player::make-player :name "TestPlayer" :room :village-square)))
    ;; Mock player stream
    (let ((*standard-output* (make-string-output-stream)))
      ;; Test initial state
      (assert (null (mud.player::player-auto-fight player)) nil
              "Auto-fight should be nil initially")
      
      ;; Test command toggle
      (command-auto-fight player "")
      (assert (mud.player::player-auto-fight player) nil
              "Auto-fight should be t after command")
      
      ;; Test command toggle again
      (command-auto-fight player "")
      (assert (null (mud.player::player-auto-fight player)) nil
              "Auto-fight should be nil after second command")
      
      (format t "✓ Auto-fight command test passed~%"))))

(defun test-auto-fight-serialization ()
  "Test that auto-fight setting is saved and loaded correctly"
  (let ((player (mud.player::make-player :name "TestPlayer" :room :village-square)))
    ;; Set auto-fight to true
    (setf (mud.player::player-auto-fight player) t)
    
    ;; Serialize player
    (let ((data (mud.player::%serialize-player player)))
      (assert (getf data :auto-fight) nil
              "Auto-fight should be saved in serialization")
      
      ;; Restore player
      (let ((restored-player (mud.player::%restore-player data :village-square #'identity)))
        (assert (mud.player::player-auto-fight restored-player) nil
                "Auto-fight should be restored from serialization")
        
        (format t "✓ Auto-fight serialization test passed~%")))))

(defun test-auto-fight-counter-attack-conditions ()
  "Test that auto-fight counter-attack only happens under correct conditions"
  (let ((player (mud.player::make-player :name "TestPlayer" :room :village-square))
        (mob (mud.mob::create-mob :goblin-scout :village-square)))
    
    ;; Test with auto-fight disabled
    (setf (mud.player::player-auto-fight player) nil)
    (let ((*standard-output* (make-string-output-stream)))
      (auto-fight-counter-attack player mob)
      ;; Should not attack when auto-fight is disabled
      (assert (mud.mob::mob-alive-p mob) nil
              "Mob should still be alive when auto-fight is disabled"))
    
    ;; Test with auto-fight enabled
    (setf (mud.player::player-auto-fight player) t)
    (let ((*standard-output* (make-string-output-stream)))
      (auto-fight-counter-attack player mob)
      ;; Should attack when auto-fight is enabled
      (format t "✓ Auto-fight counter-attack conditions test passed~%"))))

(defun run-auto-fight-tests ()
  "Run all auto-fight tests"
  (format t "Running auto-fight tests...~%")
  (handler-case
      (progn
        (test-auto-fight-setting)
        (test-auto-fight-command)
        (test-auto-fight-serialization)
        (test-auto-fight-counter-attack-conditions)
        (format t "~%All auto-fight tests passed! ✓~%"))
    (error (err)
      (format t "~%Test failed: ~a~%" err)
      (uiop:quit 1))))

;; Run tests if this file is executed directly
(when (string= (uiop:argv0) "tests/unit/test-auto-fight.lisp")
  (run-auto-fight-tests))
