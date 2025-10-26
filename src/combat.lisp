(in-package :mud.combat)

;;; Spell structure
(defstruct spell
  (name "" :type string)
  (cost 0 :type integer)
  (damage 0 :type integer)
  (description "" :type string))

;;; Global table to track corpse contents
(defparameter *corpse-data* (make-hash-table :test 'equal)
  "Maps corpse item names to their contents (list of items)")

;;; Spell definitions
(defparameter *spells*
  (list
   (make-spell :name "fireball"
               :cost 15
               :damage 25
               :description "A blazing sphere of flame erupts at your target.")
   (make-spell :name "lightning"
               :cost 20
               :damage 35
               :description "A bolt of crackling lightning strikes your foe.")
   (make-spell :name "ice-shard"
               :cost 10
               :damage 15
               :description "A frozen projectile pierces your enemy.")
   (make-spell :name "heal"
               :cost 12
               :damage -20
               :description "Warm light flows through you, mending wounds.")
   (make-spell :name "drain"
               :cost 8
               :damage 10
               :description "You siphon life energy from your target."))
  "List of available spells")

(defun find-spell (name)
  "Find a spell by name (case-insensitive)"
  (find-if (lambda (spell)
             (string-equal (spell-name spell) name))
           *spells*))

(defun create-corpse (player)
  "Create a corpse from a dead player with their inventory"
  (let* ((corpse-name (format nil "corpse-of-~a" (string-downcase (mud.player:player-name player))))
         (corpse-item (mud.inventory::make-item
                       :name corpse-name
                       :type :corpse
                       :effect :corpse-container
                       :value 0
                       :description (format nil "The lifeless body of ~a." (mud.player:player-name player)))))
    ;; Store corpse contents in the global hash table
    (setf (gethash corpse-name *corpse-data*)
          (copy-list (mud.player:player-inventory player)))
    corpse-item))

(defun loot-corpse (corpse)
  "Extract items from a corpse. Returns the list of items."
  (when (and corpse (eq (mud.inventory::item-type corpse) :corpse))
    (let* ((corpse-name (mud.inventory::item-name corpse))
           (items (gethash corpse-name *corpse-data*)))
      ;; Clear the corpse data after looting
      (remhash corpse-name *corpse-data*)
      items)))

(defun handle-player-death (player)
  "Handle player death: create corpse, drop items, move to graveyard, restore partial health"
  (let ((room-id (mud.player:player-room player))
        (corpse (create-corpse player)))
    ;; Clear player inventory
    (setf (mud.player:player-inventory player) nil)
    ;; Drop corpse in current room
    (mud.world:add-item-to-room room-id corpse)
    ;; Exit vehicle if in one
    (when (mud.player:player-vehicle player)
      (mud.world:add-item-to-room room-id (mud.player:player-vehicle player))
      (setf (mud.player:player-vehicle player) nil))
    ;; Move player to graveyard
    (mud.player:set-player-room player 'mud.world::graveyard)
    ;; Restore partial health (30%) and some mana (50%)
    (mud.player:set-player-health player (floor (* (mud.player:player-max-health player) 0.3)))
    (mud.player:set-player-mana player (floor (* (mud.player:player-max-mana player) 0.5)))))

(defun cast-spell (caster target spell-name)
  "Cast a spell from caster to target. Returns (values success message death-occurred)"
  (let ((spell (find-spell spell-name)))
    (cond
      ((null spell)
       (values nil (format nil "Unknown spell: ~a" spell-name) nil))

      ((not (mud.player:player-alive-p caster))
       (values nil "You are dead and cannot cast spells." nil))

      ((not (mud.player:player-alive-p target))
       (values nil (format nil "~a is already dead." (mud.player:player-name target)) nil))

      ((< (mud.player:player-mana caster) (spell-cost spell))
       (values nil (format nil "Not enough mana. ~a costs ~d mana."
                          (spell-name spell) (spell-cost spell)) nil))

      (t
       ;; Deduct mana
       (mud.player:modify-mana caster (- (spell-cost spell)))

       ;; Apply damage/healing
       (let ((damage (spell-damage spell)))
         (if (string-equal spell-name "heal")
             ;; Heal self
             (progn
               (mud.player:modify-health caster (abs damage))
               (values t (format nil "You cast ~a and restore ~d health!"
                               (spell-name spell) (abs damage)) nil))
             ;; Damage target
             (progn
               (mud.player:modify-health target (- damage))
               (let ((target-name (mud.player:player-name target))
                     (target-died (not (mud.player:player-alive-p target))))
                 (when target-died
                   (handle-player-death target))
                 (if target-died
                     (values t (format nil "You cast ~a at ~a for ~d damage! ~a has died!"
                                     (spell-name spell) target-name damage target-name) t)
                     (values t (format nil "You cast ~a at ~a for ~d damage!"
                                     (spell-name spell) target-name damage) nil))))))))))

(defun get-player-stats (player)
  "Return a formatted string of player stats"
  (format nil "Health: ~d/~d | Mana: ~d/~d | Level: ~d | Gold: ~d"
          (mud.player:player-health player)
          (mud.player:player-max-health player)
          (mud.player:player-mana player)
          (mud.player:player-max-mana player)
          (mud.player:player-level player)
          (mud.player:player-gold player)))

(defun respawn-player (player)
  "Respawn a dead player with full health and mana, move to village square"
  (mud.player:set-player-health player (mud.player:player-max-health player))
  (mud.player:set-player-mana player (mud.player:player-max-mana player))
  ;; Move player back to starting location
  (mud.player:set-player-room player 'mud.world::village-square))
