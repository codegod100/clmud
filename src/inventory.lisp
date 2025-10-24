(in-package :mud.inventory)

;;; Item structure
(defstruct item
  (name "" :type string)
  (type :consumable :type keyword)  ; :consumable, :equipment, :key, etc.
  (effect nil :type (or null keyword))  ; :restore-mana, :restore-health, etc.
  (value 0 :type integer)  ; Amount of effect (mana/health restored, etc.)
  (description "" :type string))

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
              :description "A glowing azure flask that fully restores mana."))
  "List of item templates")

(defun find-item-template (name)
  "Find an item template by name (case-insensitive)"
  (find-if (lambda (item)
             (string-equal (item-name item) name))
           *item-templates*))

(defun create-item (template-name)
  "Create a new item instance from a template"
  (let ((template (find-item-template template-name)))
    (when template
      (copy-item template))))

(defun add-to-inventory (player item)
  "Add an item to a player's inventory"
  (push item (mud.player:player-inventory player)))

(defun remove-from-inventory (player item)
  "Remove an item from a player's inventory"
  (setf (mud.player:player-inventory player)
        (remove item (mud.player:player-inventory player) :test #'eq)))

(defun find-in-inventory (player item-name)
  "Find the first item in player's inventory matching the name"
  (find-if (lambda (item)
             (string-equal (item-name item) item-name))
           (mud.player:player-inventory player)))

(defun list-inventory (player)
  "Return a formatted list of items in player's inventory"
  (let ((inventory (mud.player:player-inventory player)))
    (if (null inventory)
        "Your inventory is empty."
        (with-output-to-string (out)
          (format out "Inventory (~d item~:p):~%" (length inventory))
          (let ((item-counts (make-hash-table :test 'equal)))
            ;; Count items by name
            (dolist (item inventory)
              (incf (gethash (item-name item) item-counts 0)))
            ;; Display with counts
            (maphash (lambda (name count)
                       (let ((template (find-item-template name)))
                         (format out "  ~a x~d - ~a~%"
                                name count
                                (if template (item-description template) ""))))
                     item-counts))))))

(defun use-item (player item-name)
  "Use an item from player's inventory. Returns (values success message)"
  (let ((item (find-in-inventory player item-name)))
    (cond
      ((null item)
       (values nil (format nil "You don't have any ~a." item-name)))

      ((not (eq (item-type item) :consumable))
       (values nil (format nil "You can't use ~a." item-name)))

      (t
       (case (item-effect item)
         (:restore-mana
          (let ((current-mana (mud.player:player-mana player))
                (max-mana (mud.player:player-max-mana player)))
            (if (>= current-mana max-mana)
                (values nil "Your mana is already full.")
                (progn
                  (mud.player:modify-mana player (item-value item))
                  (remove-from-inventory player item)
                  (values t (format nil "You drink the ~a and restore ~d mana."
                                  item-name (item-value item)))))))

         (:restore-health
          (let ((current-health (mud.player:player-health player))
                (max-health (mud.player:player-max-health player)))
            (if (>= current-health max-health)
                (values nil "Your health is already full.")
                (progn
                  (mud.player:modify-health player (item-value item))
                  (remove-from-inventory player item)
                  (values t (format nil "You drink the ~a and restore ~d health."
                                  item-name (item-value item)))))))

         (t
          (values nil (format nil "~a has an unknown effect." item-name))))))))

(defun drop-item (player item-name)
  "Drop an item from inventory into the room. Returns (values success message)"
  (let ((item (find-in-inventory player item-name)))
    (cond
      ((null item)
       (values nil (format nil "You don't have any ~a." item-name)))
      (t
       (remove-from-inventory player item)
       (mud.world:add-item-to-room (mud.player:player-room player) item)
       (values t (format nil "You drop ~a." item-name))))))

(defun grab-item (player item-name)
  "Grab an item from the room into inventory. Returns (values success message)"
  (let ((item (mud.world:find-item-in-room (mud.player:player-room player) item-name)))
    (cond
      ((null item)
       (values nil (format nil "There is no ~a here." item-name)))
      (t
       (mud.world:remove-item-from-room (mud.player:player-room player) item)
       (add-to-inventory player item)
       (values t (format nil "You grab ~a." item-name))))))
