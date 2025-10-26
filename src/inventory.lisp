(in-package :mud.inventory)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (declaim (ftype (function (t t) *) mud.world:add-item-to-room))
  (declaim (ftype (function (t t) *) mud.world:remove-item-from-room))
  (declaim (ftype (function (t t) *) mud.world:find-item-in-room)))

;;; Item structure - define early so accessors can be inlined
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defstruct item
    name
    type
    description
    (portable t :type boolean)
    (damage 0 :type integer)
    (armor 0 :type integer)
    (vehicle-type nil)
    (effect nil)           ; For consumable effects
    (value 0 :type integer) ; For effect values (healing/mana amount)
    (slot nil)))           ; Equipment slot (:weapon or :armor)

;;; Item templates (blueprints for creating items)
(defparameter *item-templates*
  (list
   (make-item :name "mana-potion"
              :type :consumable
              :effect :restore-mana
              :value 25
              :description "A shimmering blue vial that restores 25 mana.")
   (make-item :name "health-potion"
              :type :consumable
              :effect :restore-health
              :value 30
              :description "A crimson elixir that restores 30 health.")
   (make-item :name "greater-mana-potion"
              :type :consumable
              :effect :restore-mana
              :value 50
              :description "A glowing azure flask that fully restores mana.")
   ;; Weapons
   (make-item :name "rusty-dagger"
              :type :weapon
              :damage 5
              :slot :weapon
              :description "A rusty, chipped dagger. Better than nothing.")
   (make-item :name "bone-sword"
              :type :weapon
              :damage 10
              :slot :weapon
              :description "A sword carved from ancient bones, surprisingly sharp.")
   (make-item :name "steel-sword"
              :type :weapon
              :damage 15
              :slot :weapon
              :description "A well-crafted steel sword with a keen edge.")
   (make-item :name "guardian-axe"
              :type :weapon
              :damage 25
              :slot :weapon
              :description "A massive axe imbued with the power of nature. Vines wrap around its handle.")
   ;; Armor
   (make-item :name "rusted-chainmail"
              :type :armor
              :armor 5
              :slot :armor
              :description "Old chainmail with rust spots, but still provides some protection.")
   (make-item :name "leather-armor"
              :type :armor
              :armor 10
              :slot :armor
              :description "Supple leather armor, well-maintained and flexible.")
   (make-item :name "nature-amulet"
              :type :armor
              :armor 15
              :slot :armor
              :description "A mystical amulet that pulses with natural magic, offering protection.")
   ;; Other loot
   (make-item :name "wolf-pelt"
              :type :consumable
              :description "A grey wolf pelt. Could be sold for coin.")
   (make-item :name "gold-coins"
              :type :consumable
              :value 50
              :description "A small pouch of gold coins."))
  "List of item templates")

(defun find-item-template (name)
  "Find an item template by name (case-insensitive)"
  (find-if (lambda (item)
             (string-equal (mud.inventory::item-name item) name))
           *item-templates*))

(defun duplicate-item (item)
  "Create a copy of an item"
  (mud.inventory::make-item :name (mud.inventory::item-name item)
             :type (mud.inventory::item-type item)
             :description (mud.inventory::item-description item)
             :portable (mud.inventory::item-portable item)
             :damage (mud.inventory::item-damage item)
             :armor (mud.inventory::item-armor item)
             :vehicle-type (mud.inventory::item-vehicle-type item)
             :effect (mud.inventory::item-effect item)
             :value (mud.inventory::item-value item)
             :slot (mud.inventory::item-slot item)))

(defun create-item (template-name)
  "Create a new item instance from a template"
  (let ((template (find-item-template template-name)))
    (when template
      (duplicate-item template))))

(defun currency-item-p (item)
  (string-equal (mud.inventory::item-name item) "gold-coins"))

(defun add-to-inventory (player item)
  "Add an item to a player's inventory or convert currency to gold."
  (if (currency-item-p item)
      (let* ((amount (max 1 (mud.inventory::item-value item))))
        (mud.player:modify-gold player amount)
        (values :gold amount))
      (progn
        (push item (mud.player:player-inventory player))
        (values :item item))))

(defun remove-from-inventory (player item)
  "Remove an item from a player's inventory"
  (setf (mud.player:player-inventory player)
        (remove item (mud.player:player-inventory player) :test #'eq)))

(defun fuzzy-match-item-name (item-name-full item-name-partial)
  "Check if partial name matches the full item name (supports substring matching)"
  (let ((full (string-downcase item-name-full))
        (partial (string-downcase item-name-partial)))
    (or (string-equal full partial)
        (search partial full))))

(defun find-in-inventory (player item-name)
  "Find the first item in player's inventory matching the name (supports partial matches)"
  (find-if (lambda (item)
             (fuzzy-match-item-name (mud.inventory::item-name item) item-name))
           (mud.player:player-inventory player)))

(defun find-equipped-item (player item-name)
  "Find an equipped item matching the name (supports partial matches)"
  (let ((equipped-weapon (mud.player:player-equipped-weapon player))
        (equipped-armor (mud.player:player-equipped-armor player)))
    (cond
      ((and equipped-weapon 
            (fuzzy-match-item-name (mud.inventory::item-name equipped-weapon) item-name))
       equipped-weapon)
      ((and equipped-armor 
            (fuzzy-match-item-name (mud.inventory::item-name equipped-armor) item-name))
       equipped-armor)
      (t nil))))

(defun list-inventory (player)
  "Return a formatted list of items in player's inventory with status"
  (let ((inventory (mud.player:player-inventory player)))
    (with-output-to-string (out)
      ;; Show player status first
      (format out "~a - Level ~d~%" (mud.player:player-name player) (mud.player:player-level player))
      (format out "Health: ~d/~d  Mana: ~d/~d~%" 
              (mud.player:player-health player) (mud.player:player-max-health player)
              (mud.player:player-mana player) (mud.player:player-max-mana player))
      (format out "XP: ~d/~d  (Need ~d more to level)~%" 
              (mud.player:player-xp player)
              (mud.player:xp-for-level (+ (mud.player:player-level player) 1))
              (mud.player:xp-to-next-level player))
      (format out "Damage: ~d  Armor: ~d  Gold: ~d~%" 
              (mud.player:get-player-damage player)
              (mud.player:get-player-armor player)
              (mud.player:player-gold player))
      (when (mud.player:player-equipped-weapon player)
        (format out "Weapon: ~a (~+d damage)~%"
                (mud.inventory:item-name (mud.player:player-equipped-weapon player))
                (mud.inventory:item-damage (mud.player:player-equipped-weapon player))))
      (when (mud.player:player-equipped-armor player)
        (format out "Armor: ~a (~+d armor)~%"
                (mud.inventory:item-name (mud.player:player-equipped-armor player))
                (mud.inventory:item-armor (mud.player:player-equipped-armor player))))
      (format out "~%")
      
      ;; Show inventory
      (if (null inventory)
          (if (zerop (mud.player:player-gold player))
              (format out "Your inventory is empty.")
              (format out "Your inventory is empty, but you carry ~d gold coins." (mud.player:player-gold player)))
          (progn
            (format out "Inventory (~d item~:p):~%" (length inventory))
            (format out "Coins: ~d~%" (mud.player:player-gold player))
            (let ((item-counts (make-hash-table :test 'equal))
                  (equipped-weapon (mud.player:player-equipped-weapon player))
                  (equipped-armor (mud.player:player-equipped-armor player)))
              ;; Count items by name
              (dolist (item inventory)
                (incf (gethash (mud.inventory::item-name item) item-counts 0)))
              ;; Display with counts and equipped status
              (maphash (lambda (name count)
                         (let ((template (find-item-template name)))
                           (when template
                             (let ((is-equipped-weapon (and equipped-weapon
                                                           (string= name (mud.inventory::item-name equipped-weapon))))
                                   (is-equipped-armor (and equipped-armor
                                                          (string= name (mud.inventory::item-name equipped-armor))))
                                   (item-type (mud.inventory::item-type template))
                                   (damage (mud.inventory::item-damage template))
                                   (armor (mud.inventory::item-armor template)))
                               ;; Format: name x count [EQUIPPED] - description [+damage/+armor]
                               (format out "  ~a x~d~a - ~a"
                                      name count
                                      (cond (is-equipped-weapon " [EQUIPPED]")
                                            (is-equipped-armor " [EQUIPPED]")
                                            (t ""))
                                      (mud.inventory::item-description template))
                               ;; Add stats if weapon or armor
                               (cond
                                 ((and (eq item-type :weapon) (> damage 0))
                                  (format out " [+~d damage]" damage))
                                 ((and (eq item-type :armor) (> armor 0))
                                  (format out " [+~d armor]" armor)))
                               (format out "~%")))))
                       item-counts)))))))

(defun use-item (player item-name)
  "Use an item from player's inventory. Returns (values success message)"
  (let ((item (find-in-inventory player item-name)))
    (cond
      ((null item)
       (values nil (format nil "You don't have any ~a." item-name)))

      ((not (eq (mud.inventory::item-type item) :consumable))
       (values nil (format nil "You can't use ~a." item-name)))

      (t
       (case (mud.inventory::item-effect item)
         (:restore-mana
          (let ((current-mana (mud.player:player-mana player))
                (max-mana (mud.player:player-max-mana player)))
            (if (>= current-mana max-mana)
                (values nil "Your mana is already full.")
                (progn
                  (mud.player:modify-mana player (mud.inventory::item-value item))
                  (mud.inventory::remove-from-inventory player item)
                  (values t (format nil "You drink the ~a and restore ~d mana."
                                  item-name (mud.inventory::item-value item)))))))

         (:restore-health
          (let ((current-health (mud.player:player-health player))
                (max-health (mud.player:player-max-health player)))
            (if (>= current-health max-health)
                (values nil "Your health is already full.")
                (progn
                  (mud.player:modify-health player (mud.inventory::item-value item))
                  (mud.inventory::remove-from-inventory player item)
                  (values t (format nil "You drink the ~a and restore ~d health."
                                  item-name (mud.inventory::item-value item)))))))

         (t
          (values nil (format nil "~a has an unknown effect." item-name))))))))

(defun drop-item (player item-name)
  "Drop an item from inventory into the room. Returns (values success message)"
  (let ((item (find-in-inventory player item-name)))
    (cond
      ((null item)
       (values nil (format nil "You don't have any ~a." item-name)))
      (t
       (mud.inventory::remove-from-inventory player item)
       (mud.world:add-item-to-room (mud.player:player-room player) item)
       (values t (format nil "You drop ~a." item-name))))))

(defun grab-item (player item-name)
  "Get an item from the room into inventory. Returns (values success message)"
  (let ((item (mud.world:find-item-in-room (mud.player:player-room player) item-name)))
    (cond
      ((null item)
       (values nil (format nil "There is no ~a here." item-name)))
      ((not (mud.inventory::item-portable item))
       (values nil (format nil "~a is too large to pick up." (mud.inventory::item-name item))))
      (t
       (mud.world:remove-item-from-room (mud.player:player-room player) item)
       (multiple-value-bind (kind payload)
           (add-to-inventory player item)
         (values t (if (eq kind :gold)
                       (format nil "You pick up ~d gold coins." payload)
                       (format nil "You get ~a." item-name))))))))
