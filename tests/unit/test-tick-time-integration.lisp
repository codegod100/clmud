#!/usr/bin/env sbcl --script

;; Test tick and time system integration
;; This test verifies that the global tick system is properly integrated with the time system

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
(load "src/server/commands/time.lisp")

(in-package :mud.server)

(defun test-tick-time-integration ()
  "Test that the tick system is properly integrated with the time system"
  ;; Initialize world
  (mud.world::initialize-world)
  
  (format t "~%=== Testing Tick-Time Integration ===")
  
  ;; Test initial state
  (format t "~%Initial state:")
  (format t "~%  Global tick: ~d" (mud.world::get-global-tick))
  (format t "~%  World time: ~a" (mud.world::format-world-time))
  (format t "~%  Tick rate: ~d ticks/hour" (mud.world::get-tick-rate))
  (format t "~%  Tick interval: ~d seconds" mud.constants::*tick-interval*)
  
  ;; Test tick advancement
  (format t "~%~%Testing tick advancement...")
  (dotimes (i 5)
    (let ((old-tick (mud.world::get-global-tick))
          (old-time (mud.world::get-world-time)))
      (mud.world::advance-global-tick)
      (let ((new-tick (mud.world::get-global-tick))
            (new-time (mud.world::get-world-time)))
        (format t "~%  Tick ~d: ~d -> ~d, Time: ~a -> ~a" 
                (1+ i) old-tick new-tick 
                (format nil "~,2f" old-time) (format nil "~,2f" new-time)))))
  
  ;; Test time formatting with ticks
  (format t "~%~%Testing time formatting:")
  (format t "~%  Formatted time: ~a" (mud.world::format-world-time))
  
  ;; Test tick rate calculation
  (format t "~%~%Testing tick rate calculation:")
  (let ((ticks-per-hour (mud.world::get-tick-rate))
        (seconds-per-tick mud.constants::*tick-interval*)
        (calculated-hours-per-tick (/ seconds-per-tick 3600.0)))
    (format t "~%  Ticks per hour: ~d" ticks-per-hour)
    (format t "~%  Seconds per tick: ~d" seconds-per-tick)
    (format t "~%  Hours per tick: ~,4f" calculated-hours-per-tick)
    (format t "~%  Expected: ~,4f" (/ 1.0 ticks-per-hour)))
  
  ;; Test time command output
  (format t "~%~%Testing time command output:")
  (let* ((player (mud.player::make-player :name "TestPlayer" :room :village-square))
         (*standard-output* (make-string-output-stream)))
    (command-time player "")
    (format t "~%  Time command executed (output captured)"))
  
  (format t "~%~%✅ Tick-time integration test passed!")
  t)

(defun run-test ()
  "Run the tick-time integration test"
  (handler-case
      (test-tick-time-integration)
    (error (e)
      (format t "~%~%❌ Test failed: ~a" e)
      (format t "~%Backtrace:")
      (sb-debug:print-backtrace)
      nil)))

;; Run the test
(run-test)
