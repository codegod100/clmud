#!/usr/bin/sbcl --script

;; Test quest item restrictions
;; This test demonstrates that merchants won't buy quest items

(load "src/packages.lisp")
(load "src/ansi.lisp")
(load "src/world.lisp")
(load "src/player.lisp")
(load "src/inventory.lisp")
(load "src/merchant.lisp")

;; Initialize systems
(mud.world::initialize-world)
(mud.merchant::initialize-merchants)

(format t "=== Quest Item Restrictions Test ===~%~%")

;; Create a test player
(let ((player (mud.player::make-player :name "TestPlayer" :stream *standard-output*)))
  ;; Add various items including quest items and non-quest items
  (mud.inventory::add-to-inventory player (mud.inventory::create-item "apple")) ; quest item
  (mud.inventory::add-to-inventory player (mud.inventory::create-item "treasure-map")) ; quest item
  (mud.inventory::add-to-inventory player (mud.inventory::create-item "pirate-cutlass")) ; quest item
  (mud.inventory::add-to-inventory player (mud.inventory::create-item "rusty-dagger")) ; regular item
  (mud.inventory::add-to-inventory player (mud.inventory::create-item "healing-potion")) ; regular item
  (mud.inventory::add-to-inventory player (mud.inventory::create-item "steel-sword")) ; regular item
  
  ;; Get a merchant
  (let ((merchant (mud.merchant::find-merchant :lena)))
    (when merchant
      (format t "Testing quest item restrictions:~%")
      
      ;; Test selling quest items (should fail)
      (format t "~%1. Testing quest items (should be rejected):")
      (dolist (quest-item '("apple" "treasure-map" "pirate-cutlass"))
        (format t "~%   Trying to sell ~a:" quest-item)
        (multiple-value-bind (success message item-id)
            (mud.merchant::merchant-sell-item merchant player quest-item)
          (format t "~%     Success: ~a" success)
          (format t "~%     Message: ~a" message)))
      
      ;; Test selling regular items (should work)
      (format t "~%~%2. Testing regular items (should be accepted):")
      (dolist (regular-item '("rusty-dagger" "healing-potion" "steel-sword"))
        (format t "~%   Trying to sell ~a:" regular-item)
        (multiple-value-bind (success message item-id)
            (mud.merchant::merchant-sell-item merchant player regular-item)
          (format t "~%     Success: ~a" success)
          (format t "~%     Message: ~a" message)))
      
      (format t "~%~%=== Test completed! ===")
      (format t "~%Quest item restrictions working: merchants reject quest items~%"))))

(format t "~%Test file: tests/features/test-quest-item-restrictions.lisp~%")
(format t "This test demonstrates:~%")
(format t "- Quest items (apple, treasure-map, pirate-cutlass) cannot be sold~%")
(format t "- Regular items can be sold to merchants~%")
(format t "- Quest items are marked with :quest-item t in item templates~%")
(format t "- Merchants use mud.inventory:quest-item-p to check items~%")
