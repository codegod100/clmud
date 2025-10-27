#!/usr/bin/env sbcl --script

;; Test inventory display with equipped items

(load "src/packages.lisp")
(load "src/world.lisp")
(load "src/player.lisp")
(load "src/inventory.lisp")

;; Initialize the world to get access to room functions
(mud.world:initialize-world)

(defun test-inventory-display-with-equipped-items ()
  "Test that equipped items show [EQUIPPED] status in inventory display"
  (let* ((room (mud.world:starting-room))
         (player (mud.player:make-player :name "TestPlayer" :room room))
         (weapon (mud.inventory:create-item "pirate-cutlass"))
         (armor (mud.inventory:create-item "nature-amulet")))
    
    ;; Add items to inventory
    (mud.inventory:add-to-inventory player weapon)
    (mud.inventory:add-to-inventory player armor)
    
    ;; Equip the items
    (mud.player:equip-item player weapon)
    (mud.player:equip-item player armor)
    
    ;; Get inventory display
    (let ((inventory-display (mud.inventory:list-inventory player)))
      (format t "=== Inventory Display Test ===~%")
      (format t "~a~%" inventory-display)
      
      ;; Check that equipped items are NOT shown in inventory
      (let ((has-equipped-weapon (search "pirate-cutlass" inventory-display))
            (has-equipped-armor (search "nature-amulet" inventory-display)))
        (if (and (not has-equipped-weapon) (not has-equipped-armor))
            (progn
              (format t "✅ SUCCESS: Equipped items are hidden from inventory~%")
              t)
            (progn
              (format t "❌ FAILED: Equipped items should be hidden from inventory~%")
              nil))))))

(defun test-inventory-display-without-equipped-items ()
  "Test that non-equipped items don't show [EQUIPPED] status"
  (let* ((room (mud.world:starting-room))
         (player (mud.player:make-player :name "TestPlayer" :room room))
         (weapon (mud.inventory:create-item "pirate-cutlass"))
         (armor (mud.inventory:create-item "nature-amulet")))
    
    ;; Add items to inventory but don't equip them
    (mud.inventory:add-to-inventory player weapon)
    (mud.inventory:add-to-inventory player armor)
    
    ;; Get inventory display
    (let ((inventory-display (mud.inventory:list-inventory player)))
      (format t "=== Non-Equipped Items Test ===~%")
      (format t "~a~%" inventory-display)
      
      ;; Check that items don't show [EQUIPPED] status
      (let ((has-equipped-status (search "[EQUIPPED]" inventory-display)))
        (if (not has-equipped-status)
            (progn
              (format t "✅ SUCCESS: Non-equipped items don't show [EQUIPPED] status~%")
              t)
            (progn
              (format t "❌ FAILED: Non-equipped items showing [EQUIPPED] status~%")
              nil))))))

(defun test-duplicate-equipped-items ()
  "Test that when you have an equipped weapon and another copy in inventory, the inventory shows [EQUIPPED] for the one in inventory"
  (let* ((room (mud.world:starting-room))
         (player (mud.player:make-player :name "TestPlayer" :room room))
         (weapon1 (mud.inventory:create-item "pirate-cutlass"))
         (weapon2 (mud.inventory:create-item "pirate-cutlass"))
         (armor (mud.inventory:create-item "nature-amulet")))
    
    ;; Add items to inventory
    (mud.inventory:add-to-inventory player weapon1)
    (mud.inventory:add-to-inventory player weapon2)
    (mud.inventory:add-to-inventory player armor)
    
    ;; Equip one weapon and the armor
    (mud.player:equip-item player weapon1)
    (mud.player:equip-item player armor)
    
    ;; Get inventory display
    (let ((inventory-display (mud.inventory:list-inventory player)))
      (format t "=== Duplicate Equipped Items Test ===~%")
      (format t "~a~%" inventory-display)
      
      ;; Check that we have 1 pirate-cutlass item (the unequipped one) and no [EQUIPPED] status
      (let ((cutlass-count (count-substring "pirate-cutlass" inventory-display))
            (equipped-count (count-substring "[EQUIPPED]" inventory-display)))
        (if (and (= cutlass-count 1) (= equipped-count 0))
            (progn
              (format t "✅ SUCCESS: Found 1 pirate-cutlass item (unequipped), no [EQUIPPED] status~%")
              t)
            (progn
              (format t "❌ FAILED: Expected 1 pirate-cutlass item and 0 [EQUIPPED], found ~d and ~d~%" cutlass-count equipped-count)
              nil))))))

(defun count-substring (substring string)
  "Count occurrences of substring in string"
  (let ((count 0)
        (start 0))
    (loop while (setf start (search substring string :start2 start))
          do (incf count)
             (incf start (length substring)))
    count))

(defun run-tests ()
  "Run all inventory display tests"
  (format t "Running inventory display tests...~%~%")
  
  (let ((test1-result (test-inventory-display-with-equipped-items))
        (test2-result (test-inventory-display-without-equipped-items))
        (test3-result (test-duplicate-equipped-items)))
    
    (format t "~%=== Test Results ===~%")
    (format t "Equipped items test: ~a~%" (if test1-result "PASSED" "FAILED"))
    (format t "Non-equipped items test: ~a~%" (if test2-result "PASSED" "FAILED"))
    (format t "Duplicate equipped items test: ~a~%" (if test3-result "PASSED" "FAILED"))
    
    (if (and test1-result test2-result test3-result)
        (progn
          (format t "~%🎉 ALL TESTS PASSED!~%")
          (exit :code 0))
        (progn
          (format t "~%💥 SOME TESTS FAILED!~%")
          (exit :code 1)))))

;; Run the tests
(run-tests)
