;; Test equipped items with real save file

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

;; Test with real save file
(format t "=== Real Save File Test ===~%")

;; Load the game state
(let ((restored-count (mud.server::load-game-state)))
  (format t "Restored count: ~d~%" restored-count)
  
  ;; Check if any players were loaded
  (if (> restored-count 0)
      (progn
        ;; Find the player in the registry
        (let ((player nil))
          (maphash (lambda (name p) 
                     (when (string= name "V")
                       (setf player p)))
                   mud.player::*player-registry*)
          
          (if player
              (progn
                (format t "~%Found player: ~a~%" (mud.player::player-name player))
                (format t "Equipped weapon: ~a~%" 
                        (if (mud.player::player-equipped-weapon player)
                            (mud.inventory::item-name (mud.player::player-equipped-weapon player))
                            "None"))
                (format t "Equipped armor: ~a~%" 
                        (if (mud.player::player-equipped-armor player)
                            (mud.inventory::item-name (mud.player::player-equipped-armor player))
                            "None"))
                
                (format t "~%Inventory items:~%")
                (dolist (item (mud.player::player-inventory player))
                  (format t "  ~a~%" (mud.inventory::item-name item)))
                
                (format t "~%Inventory count: ~d~%" (length (mud.player::player-inventory player)))
                
                ;; Check if equipped items are correct
                (let ((weapon-correct (and (mud.player::player-equipped-weapon player)
                                           (string= (mud.inventory::item-name (mud.player::player-equipped-weapon player)) "steel-sword")))
                      (armor-correct (and (mud.player::player-equipped-armor player)
                                          (string= (mud.inventory::item-name (mud.player::player-equipped-armor player)) "leather-armor"))))
                  
                  (if (and weapon-correct armor-correct)
                      (format t "✓ Equipped items loaded correctly!~%")
                      (progn
                        (format t "✗ Equipped items NOT loaded correctly:~%")
                        (when (not weapon-correct) (format t "  - Weapon should be steel-sword~%"))
                        (when (not armor-correct) (format t "  - Armor should be leather-armor~%"))))))
              (format t "✗ Player 'V' not found in registry~%"))))
      (format t "✗ No players were restored~%")))
