#!/usr/bin/env sbcl --script

;; Test repair command functionality
(load "src/packages.lisp")
(load "src/ansi.lisp")
(load "src/world.lisp")
(load "src/inventory.lisp")
(load "src/player.lisp")
(load "src/merchant.lisp")
(load "src/server/commands/core.lisp")
(load "src/server/commands/movement.lisp")
(load "src/server/commands/inventory.lisp")
(load "src/server/commands/vehicle.lisp")

(in-package :mud.server)

(defun test-repair-functionality ()
  "Test the repair command and vehicle broken state"
  (format t "=== Testing Repair Functionality ===~%")
  
  ;; Initialize world
  (mud.world:initialize-world)
  (mud.merchant:initialize-merchants)
  
  ;; Create a test player
  (let* ((player (mud.player:make-player :name "TestPlayer" :room 'mud.world::village-square))
         (vehicle-item (mud.inventory:create-item "boat"))
         (vehicle-template (mud.world:find-vehicle "boat")))
    
    ;; Test 1: Player enters vehicle
    (format t "Test 1: Player enters vehicle~%")
    (setf (mud.player:player-vehicle player) vehicle-item)
    (format t "✓ Player is now in vehicle: ~a~%" (mud.inventory:item-name vehicle-item))
    
    ;; Test 2: Check initial vehicle state
    (format t "Test 2: Check initial vehicle state~%")
    (let ((current-armor (mud.world:vehicle-armor vehicle-template))
          (max-armor (mud.world:vehicle-max-armor vehicle-template)))
      (format t "✓ Vehicle armor: ~d/~d~%" current-armor max-armor)
      (format t "✓ Vehicle broken: ~a~%" (mud.player:vehicle-broken-p player)))
    
    ;; Test 3: Damage vehicle to break it
    (format t "Test 3: Damage vehicle to break it~%")
    (setf (mud.world:vehicle-armor vehicle-template) 0)
    (format t "✓ Vehicle armor after damage: ~d~%" (mud.world:vehicle-armor vehicle-template))
    (format t "✓ Vehicle broken: ~a~%" (mud.player:vehicle-broken-p player))
    
    ;; Test 4: Try to move with broken vehicle (should fail)
    (format t "Test 4: Try to move with broken vehicle~%")
    (let ((result (move-player player :north)))
      (format t "✓ Movement result: ~a (should be nil)~%" result))
    
    ;; Test 5: Add repair kit to inventory
    (format t "Test 5: Add repair kit to inventory~%")
    (let ((repair-kit (mud.inventory:create-item "repair-kit")))
      (mud.inventory:add-to-inventory player repair-kit)
      (format t "✓ Repair kit added to inventory~%"))
    
    ;; Test 6: Use repair kit
    (format t "Test 6: Use repair kit~%")
    (multiple-value-bind (success message)
        (mud.inventory:use-item player "repair-kit")
      (format t "✓ Repair result: ~a~%" success)
      (format t "✓ Repair message: ~a~%" message)
      (format t "✓ Vehicle armor after repair: ~d~%" (mud.world:vehicle-armor vehicle-template))
      (format t "✓ Vehicle broken after repair: ~a~%" (mud.player:vehicle-broken-p player)))
    
    ;; Test 7: Try to move after repair (should succeed)
    (format t "Test 7: Try to move after repair~%")
    (let ((result (move-player player :north)))
      (format t "✓ Movement result: ~a (should be t)~%" result))
    
    (format t "=== All tests completed ===~%")))

;; Run the test
(test-repair-functionality)
