#!/usr/bin/sbcl --script

;; Test that the bandit's vehicle is properly displayed in room descriptions

(load "src/packages.lisp")
(load "src/world.lisp")
(load "src/mob.lisp")

(defun test-bandit-vehicle-display ()
  "Test that the bandit's vehicle is displayed correctly"
  (format t "=== Testing Bandit Vehicle Display ===~%")
  
  ;; Initialize world
  (mud.world::initialize-world)
  (mud.mob::initialize-mobs)
  
  ;; Spawn a bandit
  (let ((bandit (mud.mob::spawn-mob :bandit 'mud.world::moonlit-lane)))
    (assert bandit nil "Bandit should be spawned successfully")
    
    ;; Check that the bandit has a vehicle
    (let ((vehicle (mud.mob::mob-vehicle bandit)))
      (assert (string-equal vehicle "motorcycle") nil
              "Bandit should have motorcycle vehicle, got: ~a" vehicle)
      (format t "✓ Bandit has motorcycle vehicle: ~a~%" vehicle))
    
    ;; Test the mob display format
    (let ((mob-name (mud.mob::mob-name bandit))
          (vehicle (mud.mob::mob-vehicle bandit)))
      (let ((display-format (if vehicle
                                (format nil "~a [on ~a]" mob-name vehicle)
                                mob-name)))
        (assert (search "[on motorcycle]" display-format) nil
                "Mob display should show vehicle: ~a" display-format)
        (format t "✓ Mob display format includes vehicle: ~a~%" display-format)))
    
    ;; Test individual mob inspection format
    (let ((vehicle (mud.mob::mob-vehicle bandit)))
      (let ((inspection-format (format nil "~a: ~a~%Health: ~d/~d  Damage: ~d  Armor: ~d~@[~%Vehicle: ~a~]"
                                       (mud.mob::mob-name bandit)
                                       (mud.mob::mob-description bandit)
                                       (mud.mob::mob-health bandit)
                                       (mud.mob::mob-max-health bandit)
                                       (mud.mob::mob-damage bandit)
                                       (mud.mob::mob-armor bandit)
                                       vehicle)))
        (assert (search "Vehicle: motorcycle" inspection-format) nil
                "Mob inspection should show vehicle: ~a" inspection-format)
        (format t "✓ Mob inspection format includes vehicle: ~a~%" inspection-format)))
  
  (format t "✓ All tests passed! Bandit vehicle display works correctly.~%"))

;; Run the test
(handler-case
    (test-bandit-vehicle-display)
  (error (e)
    (format t "❌ Test failed: ~a~%" e)
    (sb-ext:quit :unix-status 1)))

(format t "~%Test completed successfully!~%")
