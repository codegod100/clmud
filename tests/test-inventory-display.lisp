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
      
      ;; Check that equipped items appear with [EQUIPPED]
      (let ((equipped-weapon (count-substring "Currently equipped weapon" inventory-display))
            (equipped-armor (count-substring "Currently equipped armor" inventory-display)))
        (if (and (= equipped-weapon 1)
                 (= equipped-armor 1))
            (progn
              (format t "✅ SUCCESS: Equipped items displayed separately~%")
              t)
            (progn
              (format t "❌ FAILED: Equipped items not shown separately (weapon ~d, armor ~d)~%"
                      equipped-weapon equipped-armor)
              nil)))))

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
      
      ;; Expect duplicate to show one inventory copy plus equipped summaries
      (let ((cutlass-total (count-substring "pirate-cutlass" inventory-display))
            (equipped-weapon (count-substring "Currently equipped weapon" inventory-display))
            (equipped-tags (count-substring "[EQUIPPED]" inventory-display)))
        (if (and (>= cutlass-total 1)
                 (= equipped-weapon 1)
                 (>= equipped-tags 1))
            (progn
              (format t "✅ SUCCESS: Inventory shows duplicates and equipped summary~%")
              t)
            (progn
              (format t "❌ FAILED: Inventory/equipped summary mismatch (total ~d, equip-lines ~d, tags ~d)~%"
                      cutlass-total equipped-weapon equipped-tags)
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
)
