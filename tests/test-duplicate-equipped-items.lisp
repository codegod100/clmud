#!/usr/bin/env sbcl --script

;; Test inventory display with equipped items and duplicates in inventory

(load "src/packages.lisp")
(load "src/world.lisp")
(load "src/player.lisp")
(load "src/inventory.lisp")

;; Initialize the world to get access to room functions
(mud.world:initialize-world)

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
      
      ;; Check that we have 2 pirate-cutlass items
      (let ((cutlass-count (count-substring "pirate-cutlass" inventory-display)))
        (if (= cutlass-count 2)
            (progn
              (format t "✅ SUCCESS: Found 2 pirate-cutlass items in inventory~%")
              ;; Check that one shows [EQUIPPED] and one doesn't
              (let ((equipped-count (count-substring "[EQUIPPED]" inventory-display)))
                (if (= equipped-count 2) ; weapon + armor
                    (progn
                      (format t "✅ SUCCESS: Both equipped items show [EQUIPPED] status~%")
                      t)
                    (progn
                      (format t "❌ FAILED: Expected 2 [EQUIPPED] items, found ~d~%" equipped-count)
                      nil)))
            (progn
              (format t "❌ FAILED: Expected 2 pirate-cutlass items, found ~d~%" cutlass-count)
              nil))))))

(defun count-substring (substring string)
  "Count occurrences of substring in string"
  (let ((count 0)
        (start 0))
    (loop while (setf start (search substring string :start2 start))
          do (incf count)
             (incf start (length substring)))
    count))

(defun run-test ()
  "Run the duplicate equipped items test"
  (format t "Running duplicate equipped items test...~%~%")
  
  (let ((result (test-duplicate-equipped-items)))
    (format t "~%=== Test Result ===~%")
    (format t "Duplicate equipped items test: ~a~%" (if result "PASSED" "FAILED"))
    
    (if result
        (progn
          (format t "~%🎉 TEST PASSED!~%")
          (exit :code 0))
        (progn
          (format t "~%💥 TEST FAILED!~%")
          (exit :code 1)))))

;; Run the test
(run-test)

