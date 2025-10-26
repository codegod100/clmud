#!/usr/bin/sbcl --script

;; Test that the bandit has a motorcycle vehicle

(load "src/packages.lisp")
(load "src/world.lisp")
(load "src/mob.lisp")

(defun test-bandit-motorcycle ()
  "Test that the bandit has a motorcycle vehicle"
  (format t "=== Testing Bandit Motorcycle ===~%")
  
  ;; Initialize world
  (mud.world::initialize-world)
  (mud.mob::initialize-mobs)
  
  ;; Check bandit template
  (let ((bandit-template (mud.mob::find-mob-template :bandit)))
    (assert bandit-template nil "Bandit template should exist")
    
    ;; Check vehicle
    (let ((vehicle (mud.mob::mob-vehicle bandit-template)))
      (assert (string-equal vehicle "motorcycle") nil 
              "Bandit should have motorcycle vehicle, got: ~a" vehicle)
      (format t "✓ Bandit has motorcycle vehicle: ~a~%" vehicle))
    
    ;; Check description mentions motorcycle
    (let ((description (mud.mob::mob-description bandit-template)))
      (assert (search "motorcycle" description :test #'char-equal) nil
              "Bandit description should mention motorcycle")
      (format t "✓ Bandit description mentions motorcycle~%"))
    
    ;; Check loot table includes motorcycle
    (let ((loot-table (mud.mob::mob-loot-table bandit-template)))
      (assert (member "motorcycle" loot-table :test #'string-equal) nil
              "Bandit loot table should include motorcycle")
      (format t "✓ Bandit loot table includes motorcycle~%")))
  
  ;; Check motorcycle vehicle exists
  (let ((motorcycle-vehicle (mud.world::find-vehicle "motorcycle")))
    (assert motorcycle-vehicle nil "Motorcycle vehicle should exist")
    (format t "✓ Motorcycle vehicle defined: ~a~%" (mud.world::vehicle-name motorcycle-vehicle))
    (format t "  Type: ~a~%" (mud.world::vehicle-type motorcycle-vehicle))
    (format t "  Description: ~a~%" (mud.world::vehicle-description motorcycle-vehicle)))
  
  (format t "✓ All tests passed! Bandit has motorcycle vehicle.~%"))

;; Run the test
(handler-case
    (test-bandit-motorcycle)
  (error (e)
    (format t "❌ Test failed: ~a~%" e)
    (sb-ext:quit :unix-status 1)))

(format t "~%Test completed successfully!~%")
