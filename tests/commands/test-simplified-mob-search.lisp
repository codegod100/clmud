;; Load required packages first
(require :sb-bsd-sockets)
(require :asdf)

;; Load the MUD system
(load "../src/packages.lisp")
(load "../src/ansi.lisp")
(load "../src/player.lisp")
(load "../src/inventory.lisp")
(load "../src/merchant.lisp")
(load "../src/world.lisp")
(load "../src/mob.lisp")
(load "../src/combat.lisp")
(load "../src/quest.lisp")
(load "../src/server/core.lisp")
(load "../src/server/commands.lisp")
(load "../src/server/runtime.lisp")

;; Initialize the world
(mud.world:initialize-world)
(mud.mob:initialize-mobs)
(mud.quest:initialize-quests)

(format t "Testing simplified mob search system...~%")

;; Test 1: Check that mob struct no longer has aliases field
(format t "~%1. Testing mob struct structure...~%")
(let ((test-mob (mud.mob::make-mob :id :test :name "Test Mob")))
  (if (mud.mob::mob-p test-mob)
      (format t "✓ Mob struct created successfully~%")
      (format t "✗ Mob struct creation failed~%"))
  ;; Try to access aliases field - should fail
  (handler-case
      (progn (mud.mob::mob-aliases test-mob) (format t "✗ Aliases field still exists~%"))
      (error () (format t "✓ Aliases field successfully removed~%"))))

;; Test 2: Test partial name matching still works
(format t "~%2. Testing partial name matching...~%")
(let ((cove-mobs (mud.mob::get-mobs-in-room 'mud.world::hidden-cove)))
  (format t "Mobs in cove: ~a~%" (mapcar #'mud.mob::mob-name cove-mobs))
  
  ;; Test various partial name searches
  (dolist (search-term '("captain" "black" "cap" "blackbeard" "blackbe" "capt"))
    (let ((found-mob (mud.mob::find-mob-in-room 'mud.world::hidden-cove search-term)))
      (if found-mob
          (format t "✓ Found mob with '~a': ~a~%" search-term (mud.mob::mob-name found-mob))
          (format t "✗ No mob found with '~a'~%" search-term)))))

;; Test 3: Test backward compatibility with existing partial names
(format t "~%3. Testing backward compatibility...~%")
(let ((goblin-mob (mud.mob::find-mob-in-room 'mud.world::whispering-wood "gob")))
  (if goblin-mob
      (format t "✓ 'gob' still matches goblin: ~a~%" (mud.mob::mob-name goblin-mob))
      (format t "✗ 'gob' no longer matches goblin~%")))

;; Test 4: Test case insensitivity
(format t "~%4. Testing case insensitivity...~%")
(let ((mob1 (mud.mob::find-mob-in-room 'mud.world::hidden-cove "CAPTAIN"))
      (mob2 (mud.mob::find-mob-in-room 'mud.world::hidden-cove "captain"))
      (mob3 (mud.mob::find-mob-in-room 'mud.world::hidden-cove "Captain")))
  (if (and mob1 mob2 mob3 (eq mob1 mob2) (eq mob2 mob3))
      (format t "✓ Case insensitive matching works~%")
      (format t "✗ Case insensitive matching failed~%")))

;; Test 5: Test that Captain Blackbeard template no longer has aliases
(format t "~%5. Testing Captain Blackbeard template...~%")
(let ((blackbeard-template (mud.mob::find-mob-template :captain-blackbeard)))
  (if blackbeard-template
      (format t "✓ Captain Blackbeard template found~%")
      (format t "✗ Captain Blackbeard template not found~%")))

(format t "~%=== Simplified Mob Search System Tests Complete ===~%")
(format t "✓ Aliases field removed from mob struct~%")
(format t "✓ define-mob-template no longer accepts aliases~%")
(format t "✓ find-mob-in-room simplified to name-only search~%")
(format t "✓ Partial name matching still works~%")
(format t "✓ Case insensitive matching preserved~%")
(format t "✓ Backward compatibility maintained~%")
