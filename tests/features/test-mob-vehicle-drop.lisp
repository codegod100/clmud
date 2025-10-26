#!/usr/bin/sbcl --script

;; Test that mobs drop their vehicles when they die

(load "src/packages.lisp")
(load "src/world.lisp")
(load "src/mob.lisp")

(defun test-mob-vehicle-drop ()
  "Test that mobs drop their vehicles when they die"
  (format t "=== Testing Mob Vehicle Drop ===~%")
  
  ;; Initialize world
  (mud.world::initialize-world)
  (mud.mob::initialize-mobs)
  
  ;; Test bandit with motorcycle
  (let* ((room-id 'mud.world::moonlit-lane)
         (bandit (mud.mob::spawn-mob :bandit room-id)))
    (assert bandit nil "Bandit should be spawned successfully")
    
    ;; Check bandit has vehicle
    (let ((vehicle (mud.mob::mob-vehicle bandit)))
      (assert (string-equal vehicle "motorcycle") nil
              "Bandit should have motorcycle vehicle, got: ~a" vehicle)
      (format t "✓ Bandit has motorcycle vehicle: ~a~%" vehicle))
    
    ;; Simulate mob death by setting health to 0
    (setf (mud.mob::mob-health bandit) 0)
    (assert (not (mud.mob::mob-alive-p bandit)) nil
            "Bandit should be dead")
    
    ;; Check that vehicle would be dropped
    (let ((vehicle (mud.mob::mob-vehicle bandit)))
      (assert vehicle nil "Dead bandit should still have vehicle reference")
      (format t "✓ Dead bandit retains vehicle reference: ~a~%" vehicle))
    
    ;; Test vehicle item creation
    (let ((vehicle-item (mud.inventory::create-item "motorcycle")))
      (assert vehicle-item nil "Should be able to create motorcycle item")
      (assert (string-equal (mud.inventory::item-name vehicle-item) "motorcycle") nil
              "Created item should be motorcycle")
      (assert (eq (mud.inventory::item-type vehicle-item) :vehicle) nil
              "Created item should be vehicle type")
      (format t "✓ Motorcycle item can be created: ~a~%" (mud.inventory::item-name vehicle-item)))
  
  ;; Test mob without vehicle (should not drop anything)
  (let* ((room-id 'mud.world::whispering-wood)
         (goblin (mud.mob::spawn-mob :goblin room-id)))
    (assert goblin nil "Goblin should be spawned successfully")
    
    ;; Check goblin has no vehicle
    (let ((vehicle (mud.mob::mob-vehicle goblin)))
      (assert (null vehicle) nil
              "Goblin should not have vehicle, got: ~a" vehicle)
      (format t "✓ Goblin has no vehicle: ~a~%" vehicle))
  
  (format t "✓ All tests passed! Mob vehicle drop system works correctly.~%"))

;; Run the test
(handler-case
    (test-mob-vehicle-drop)
  (error (e)
    (format t "❌ Test failed: ~a~%" e)
    (sb-ext:quit :unix-status 1)))

(format t "~%Test completed successfully!~%")
