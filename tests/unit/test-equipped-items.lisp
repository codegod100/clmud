;; Test equipped items serialization and deserialization

(in-package :cl-user)

;; Load the MUD system
(load "src/packages.lisp")
(load "src/ansi.lisp")
(load "src/player.lisp")
(load "src/inventory.lisp")
(load "src/world.lisp")
(load "src/server/core.lisp")
(load "src/server/runtime.lisp")

;; Initialize world
(mud.world::initialize-world)

;; Test equipped items serialization/deserialization
(let ((test-file "test-equipped.lisp"))
  
  (format t "=== Equipped Items Test ===~%")
  
  ;; Create a test player with equipped items
  (let ((player (mud.player::make-player :name "test" :room 'village-square)))
    
    ;; Add some items to inventory
    (mud.inventory::add-to-inventory player (mud.inventory::create-item "steel-sword"))
    (mud.inventory::add-to-inventory player (mud.inventory::create-item "leather-armor"))
    
    ;; Equip items
    (let ((sword (mud.inventory::find-in-inventory player "steel-sword"))
          (armor (mud.inventory::find-in-inventory player "leather-armor")))
      (when sword (mud.player::equip-item player sword))
      (when armor (mud.player::equip-item player armor)))
    
    ;; Check initial state
    (format t "Initial equipped weapon: ~a~%" (mud.player::player-equipped-weapon player))
    (format t "Initial equipped armor: ~a~%" (mud.player::player-equipped-armor player))
    
    ;; Serialize player
    (let ((serialized (mud.player::%serialize-player player)))
      (format t "~%Serialized data:~%")
      (format t "Weapon index: ~a~%" (getf serialized :equipped-weapon-index))
      (format t "Armor index: ~a~%" (getf serialized :equipped-armor-index))
      (format t "Weapon name: ~a~%" (getf serialized :equipped-weapon-name))
      (format t "Armor name: ~a~%" (getf serialized :equipped-armor-name))
      
      ;; Create a new player and restore from serialized data
      (let ((new-player (mud.player::%restore-player serialized 'village-square #'mud.world::find-room)))
        
        ;; Check restored state
        (format t "~%Restored equipped weapon: ~a~%" (mud.player::player-equipped-weapon new-player))
        (format t "Restored equipped armor: ~a~%" (mud.player::player-equipped-armor new-player))
        
        ;; Test if equipped items are correct
        (let ((weapon-correct (and (mud.player::player-equipped-weapon new-player)
                                   (string= (mud.inventory::item-name (mud.player::player-equipped-weapon new-player)) "steel-sword")))
              (armor-correct (and (mud.player::player-equipped-armor new-player)
                                  (string= (mud.inventory::item-name (mud.player::player-equipped-armor new-player)) "leather-armor"))))
          
          (if (and weapon-correct armor-correct)
              (format t "✓ Equipped items test PASSED~%")
              (format t "✗ Equipped items test FAILED~%")))))))
