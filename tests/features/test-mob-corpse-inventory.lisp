#!/usr/bin/env sbcl --script

;; Test that mobs leave corpses with their inventory when killed

(load "test-runner.lisp")

(in-package :mud.tests)

(test mob-corpse-contains-inventory
  "Test that mobs leave corpses with their inventory when killed"
  (reset-test-state)
  (let* ((room (ensure-test-room))
         (mob (make-test-mob :id :test-mob
                             :name "test mob"
                             :health 10
                             :damage 5
                             :armor 0
                             :xp 25
                             :loot '()))
         (test-item (mud.inventory::create-item "rusty-dagger"))
         (corpse nil))
    
    ;; Place mob in room
    (place-mob-in-room mob room)
    
    ;; Add item to mob's inventory
    (mud.mob::add-item-to-mob-inventory mob test-item)
    
    ;; Verify mob has the item
    (is (= 1 (length (mud.mob::mob-inventory mob))))
    (is (eq test-item (first (mud.mob::mob-inventory mob))))
    
    ;; Kill the mob
    (mud.mob::damage-mob mob 1000)
    (is (not (mud.mob::mob-alive-p mob)))
    
    ;; Create corpse from dead mob
    (setf corpse (mud.combat::create-mob-corpse mob))
    (is corpse "Should create corpse from dead mob")
    
    ;; Verify corpse contains the mob's inventory
    (let ((corpse-items (mud.combat::loot-corpse corpse)))
      (is (= 1 (length corpse-items)))
      (is (eq test-item (first corpse-items))))))

;; Run the test
(format t "~&Running mob corpse inventory test...~%")
(run! 'mob-corpse-contains-inventory)
(format t "✓ Mob corpse inventory test completed~%")
(sb-ext:exit :code 0)
