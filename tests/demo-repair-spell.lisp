#!/usr/bin/env sbcl --script

;; Demonstration script showing the repair spell functionality
;; This shows how the repair spell works in addition to repair kits

(load "src/packages.lisp")
(load "src/world.lisp")
(load "src/player.lisp")
(load "src/inventory.lisp")
(load "src/combat.lisp")

(defun demo-repair-spell ()
  "Demonstrate the repair spell functionality"
  (format t "=== Repair Spell Demonstration ===~%~%")
  
  ;; Create a player
  (let* ((player (mud.player:make-player :name "DemoPlayer"))
         (vehicle-item (mud.inventory:make-item :name "motorcycle" :type :vehicle)))
    
    ;; Set up player with high mana
    (setf (mud.player:player-mana player) 100)
    (setf (mud.player:player-vehicle player) vehicle-item)
    
    ;; Define a motorcycle vehicle
    (mud.world:define-vehicle "motorcycle" :ground "A sleek black motorcycle" :armor 12)
    
    (let ((vehicle-template (mud.world:find-vehicle "motorcycle")))
      ;; Show initial state
      (format t "Initial vehicle state:~%")
      (format t "  Vehicle: ~a~%" (mud.inventory:item-name vehicle-item))
      (format t "  Armor: ~d/~d~%" 
              (mud.world:vehicle-armor vehicle-template)
              (mud.world:vehicle-max-armor vehicle-template))
      (format t "  Player mana: ~d/~d~%" 
              (mud.player:player-mana player)
              (mud.player:player-max-mana player))
      (format t "~%")
      
      ;; Damage the vehicle
      (format t "Damaging the vehicle...~%")
      (setf (mud.world:vehicle-armor vehicle-template) 3)
      (format t "  Armor after damage: ~d/~d~%" 
              (mud.world:vehicle-armor vehicle-template)
              (mud.world:vehicle-max-armor vehicle-template))
      (format t "~%")
      
      ;; Show available repair methods
      (format t "Available repair methods:~%")
      (format t "1. Repair Kit (consumable item)~%")
      (format t "2. Repair Spell (magical)~%")
      (format t "~%")
      
      ;; Demonstrate repair spell
      (format t "Casting repair spell...~%")
      (multiple-value-bind (success message death-occurred)
          (mud.combat:cast-spell player player "repair")
        (format t "  Result: ~a~%" message)
        (format t "  Armor after repair: ~d/~d~%" 
                (mud.world:vehicle-armor vehicle-template)
                (mud.world:vehicle-max-armor vehicle-template))
        (format t "  Player mana after: ~d/~d~%" 
                (mud.player:player-mana player)
                (mud.player:player-max-mana player)))
      
      (format t "~%")
      
      ;; Show spell information
      (let ((repair-spell (mud.combat:find-spell "repair")))
        (format t "Repair Spell Details:~%")
        (format t "  Name: ~a~%" (mud.combat:spell-name repair-spell))
        (format t "  Cost: ~d mana~%" (mud.combat:spell-cost repair-spell))
        (format t "  Description: ~a~%" (mud.combat:spell-description repair-spell)))
      
      (format t "~%")
      
      ;; Show usage instructions
      (format t "How to use the repair spell:~%")
      (format t "1. Enter a vehicle (use 'enter <vehicle-name>')~%")
      (format t "2. Cast the repair spell (use 'cast repair')~%")
      (format t "3. The spell will fully restore your vehicle's armor~%")
      (format t "4. Costs 20 mana per cast~%")
      (format t "~%")
      
      ;; Compare with repair kits
      (format t "Repair Kit vs Repair Spell:~%")
      (format t "  Repair Kit:~%")
      (format t "    - Consumable item (one-time use)~%")
      (format t "    - Must be carried in inventory~%")
      (format t "    - Can be used with 'repair' command~%")
      (format t "    - No mana cost~%")
      (format t "  Repair Spell:~%")
      (format t "    - Magical ability (reusable)~%")
      (format t "    - No inventory space needed~%")
      (format t "    - Use 'cast repair' command~%")
      (format t "    - Costs 20 mana per use~%")
      (format t "~%")
      
      (format t "=== Demonstration Complete ===~%"))))

;; Run the demonstration
(demo-repair-spell)
