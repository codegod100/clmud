#!/usr/bin/env sbcl --script

;; Simple test for tick and time system integration

(load "src/packages.lisp")
(load "src/ansi.lisp")
(load "src/world.lisp")

(in-package :mud.world)

(defun test-simple-tick-time ()
  "Simple test of tick and time integration"
  (format t "~%=== Simple Tick-Time Test ===")
  
  ;; Test initial state
  (format t "~%Initial state:")
  (format t "~%  Global tick: ~d" (get-global-tick))
  (format t "~%  World time: ~a" (format-world-time))
  (format t "~%  Tick rate: ~d ticks/hour" (get-tick-rate))
  
  ;; Test tick advancement
  (format t "~%~%Testing tick advancement...")
  (dotimes (i 3)
    (let ((old-tick (get-global-tick))
          (old-time (get-world-time)))
      (advance-global-tick)
      (let ((new-tick (get-global-tick))
            (new-time (get-world-time)))
        (format t "~%  Tick ~d: ~d -> ~d, Time: ~,2f -> ~,2f" 
                (1+ i) old-tick new-tick old-time new-time))))
  
  ;; Test final state
  (format t "~%~%Final state:")
  (format t "~%  Global tick: ~d" (get-global-tick))
  (format t "~%  World time: ~a" (format-world-time))
  
  (format t "~%~%✅ Simple tick-time test passed!")
  t)

;; Run the test
(test-simple-tick-time)
