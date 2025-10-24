(in-package :mud.combat)

;;; Spell structure
(defstruct spell
  (name "" :type string)
  (cost 0 :type integer)
  (damage 0 :type integer)
  (description "" :type string))

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

(defun cast-spell (caster target spell-name)
  "Cast a spell from caster to target. Returns (values success message)"
  (let ((spell (find-spell spell-name)))
    (cond
      ((null spell)
       (values nil (format nil "Unknown spell: ~a" spell-name)))

      ((not (mud.player:player-alive-p caster))
       (values nil "You are dead and cannot cast spells."))

      ((not (mud.player:player-alive-p target))
       (values nil (format nil "~a is already dead." (mud.player:player-name target))))

      ((< (mud.player:player-mana caster) (spell-cost spell))
       (values nil (format nil "Not enough mana. ~a costs ~d mana."
                          (spell-name spell) (spell-cost spell))))

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
                               (spell-name spell) (abs damage))))
             ;; Damage target
             (progn
               (mud.player:modify-health target (- damage))
               (let ((target-name (mud.player:player-name target)))
                 (if (mud.player:player-alive-p target)
                     (values t (format nil "You cast ~a at ~a for ~d damage!"
                                     (spell-name spell) target-name damage))
                     (values t (format nil "You cast ~a at ~a for ~d damage! ~a has died!"
                                     (spell-name spell) target-name damage target-name)))))))))))

(defun get-player-stats (player)
  "Return a formatted string of player stats"
  (format nil "Health: ~d/~d | Mana: ~d/~d | Level: ~d"
          (mud.player:player-health player)
          (mud.player:player-max-health player)
          (mud.player:player-mana player)
          (mud.player:player-max-mana player)
          (mud.player:player-level player)))

(defun respawn-player (player)
  "Respawn a dead player with full health and mana"
  (mud.player:set-player-health player (mud.player:player-max-health player))
  (mud.player:set-player-mana player (mud.player:player-max-mana player)))
