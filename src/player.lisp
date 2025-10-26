(in-package :mud.player)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (declaim (ftype (function (t) integer) mud.inventory:item-damage)
           (ftype (function (t) integer) mud.inventory:item-armor)
           (ftype (function (t) keyword) mud.inventory:item-type)
           (ftype (function (t) (or keyword null)) mud.inventory:item-slot)
           (ftype (function (t) string) mud.inventory:item-name)
           (ftype (function (t) string) mud.inventory:item-description)
           (ftype (function (t) boolean) mud.inventory:item-portable)
           (ftype (function (t) (or keyword null)) mud.inventory:item-effect)
           (ftype (function (t) integer) mud.inventory:item-value)
           (ftype (function (t) (or keyword null)) mud.inventory:item-vehicle-type)
           (ftype (function (&rest t) t) mud.inventory:make-item)
           (ftype (function (t t) *) mud.inventory:add-to-inventory)))

(defclass player ()
  ((name :initarg :name :accessor player-name)
   (room :initarg :room :accessor player-room)
   (stream :initarg :stream :accessor player-stream)
   (socket :initarg :socket :accessor player-socket)
   (vehicle :initarg :vehicle :accessor player-vehicle :initform nil)
   (health :initarg :health :accessor player-health :initform 100)
   (max-health :initarg :max-health :accessor player-max-health :initform 100)
   (mana :initarg :mana :accessor player-mana :initform 50)
   (max-mana :initarg :max-mana :accessor player-max-mana :initform 50)
   (level :initarg :level :accessor player-level :initform 1)
   (xp :initarg :xp :accessor player-xp :initform 0)
   (inventory :initarg :inventory :accessor player-inventory :initform nil)
   (quest-state :initarg :quest-state :accessor player-quest-state :initform nil)
   (gold :initarg :gold :accessor player-gold :initform 0)
   ;; Equipment slots
   (equipped-weapon :initarg :equipped-weapon :accessor player-equipped-weapon :initform nil)
   (equipped-armor :initarg :equipped-armor :accessor player-equipped-armor :initform nil))
  (:documentation "Represents a connected adventurer."))

(defun make-player (&key name room stream socket)
  (make-instance 'player :name name :room room :stream stream :socket socket
                         :health 100 :max-health 100
                         :mana 50 :max-mana 50
                         :level 1
                         :xp 0
                         :inventory nil
                         :quest-state nil
                         :gold 100
                         :equipped-weapon nil
                         :equipped-armor nil))

(defparameter *player-registry* (make-hash-table :test #'equal))

(defparameter *player-registry-lock*
  (sb-thread:make-mutex :name "player-registry-lock"))

(defun %normalize-player-key (name)
  (string-upcase name))

(defun get-or-create-player (&key name room stream socket)
  "Return existing player by NAME or create a new one. Second value is T when freshly created."
  (when (null name)
    (error "Player name required."))
  (let ((key (%normalize-player-key name)))
    (sb-thread:with-mutex (*player-registry-lock*)
      (multiple-value-bind (player present) (gethash key *player-registry*)
        (if present
            (progn
              (when stream (setf (player-stream player) stream))
              (when socket (setf (player-socket player) socket))
              (when (and room (null (player-room player)))
                (setf (player-room player) room))
              (values player nil))
            (progn
              (when (null room)
                (error "Room required when creating new player ~a." name))
              (let ((new-player (make-player :name name :room room
                                             :stream stream :socket socket)))
                (setf (gethash key *player-registry*) new-player)
                (values new-player t))))))))

(defun detach-player (player)
  "Drop transport handles for PLAYER while leaving game state intact."
  (when player
    (setf (player-stream player) nil
          (player-socket player) nil)))

(defun set-player-room (player new-room)
  (setf (player-room player) new-room))

(defun set-player-health (player value)
  (setf (player-health player) (max 0 (min value (player-max-health player)))))

(defun set-player-mana (player value)
  (setf (player-mana player) (max 0 (min value (player-max-mana player)))))

(defun set-player-gold (player value)
  (setf (player-gold player) (max 0 value)))

(defun modify-health (player delta)
  "Add or subtract health from player, clamping to 0..max-health"
  (set-player-health player (+ (player-health player) delta))
  ;; Auto-show status when health changes
  (when (and (player-stream player) (not (zerop delta)))
    (show-simple-status player)))

(defun modify-mana (player delta)
  "Add or subtract mana from player, clamping to 0..max-mana"
  (set-player-mana player (+ (player-mana player) delta))
  ;; Auto-show status when mana changes
  (when (and (player-stream player) (not (zerop delta)))
    (show-simple-status player)))

(defun modify-gold (player delta)
  "Adjust player gold, never letting it drop below zero."
  (set-player-gold player (+ (player-gold player) delta)))

(defun player-p (object)
  "Check if an object is a player"
  (typep object 'player))

(defun player-alive-p (player)
  "Check if player is alive"
  (> (player-health player) 0))

(defun xp-for-level (level)
  "Calculate XP required to reach the given level"
  (* level 100))

(defun award-xp (player amount)
  "Award XP to player and handle level-ups"
  (setf (player-xp player) (+ (player-xp player) amount))
  (let ((leveled-up nil))
    (loop while (>= (player-xp player) (xp-for-level (+ (player-level player) 1)))
          do (progn
               (setf (player-level player) (+ (player-level player) 1))
               (setf leveled-up t)
               ;; Increase stats on level up
               (setf (player-max-health player) (+ (player-max-health player) 10))
               (setf (player-health player) (player-max-health player))
               (setf (player-max-mana player) (+ (player-max-mana player) 5))
               (setf (player-mana player) (player-max-mana player))))
    ;; Auto-show status when XP is awarded or player levels up
    (when (and (player-stream player) (or (not (zerop amount)) leveled-up))
      (show-simple-status player))
    leveled-up))

(defun xp-to-next-level (player)
  "Calculate XP needed for next level"
  (- (xp-for-level (+ (player-level player) 1))
     (player-xp player)))

(defun get-player-damage (player)
  "Calculate total damage (base + weapon bonus)"
  (let ((base-damage 10)
        (weapon-damage 0))
    (when (player-equipped-weapon player)
      (setf weapon-damage (mud.inventory:item-damage (player-equipped-weapon player))))
    (+ base-damage weapon-damage)))

(defun get-player-armor (player)
  "Calculate total armor rating"
  (let ((armor-rating 0))
    (when (player-equipped-armor player)
      (setf armor-rating (mud.inventory:item-armor (player-equipped-armor player))))
    armor-rating))

(defun get-vehicle-armor (player)
  "Get vehicle armor rating if player is in a vehicle"
  (if (player-vehicle player)
      (let* ((vehicle-item (player-vehicle player))
             (vehicle-template (mud.world::find-vehicle (mud.inventory:item-name vehicle-item))))
        (if vehicle-template
            (mud.world::vehicle-armor vehicle-template)
            0))
      0))

(defun damage-vehicle (player damage)
  "Apply damage to player's vehicle, return remaining damage if vehicle breaks"
  (if (player-vehicle player)
      (let* ((vehicle-item (player-vehicle player))
             (vehicle-template (mud.world::find-vehicle (mud.inventory:item-name vehicle-item))))
        (if vehicle-template
            (let ((current-armor (mud.world::vehicle-armor vehicle-template)))
              (if (>= damage current-armor)
                  ;; Vehicle breaks
                  (progn
                    (setf (mud.world::vehicle-armor vehicle-template) 0)
                    (- damage current-armor)) ; Return excess damage
                  ;; Vehicle absorbs damage
                  (progn
                    (setf (mud.world::vehicle-armor vehicle-template) (- current-armor damage))
                    0))) ; No excess damage
            damage)) ; No vehicle template found, return full damage
      damage)) ; No vehicle, return full damage

(defun vehicle-broken-p (player)
  "Check if player's vehicle is broken (armor = 0)"
  (if (player-vehicle player)
      (let* ((vehicle-item (player-vehicle player))
             (vehicle-template (mud.world::find-vehicle (mud.inventory:item-name vehicle-item))))
        (if vehicle-template
            (zerop (mud.world::vehicle-armor vehicle-template))
            t)) ; No template found, consider it broken
      nil)) ; No vehicle, not broken

(defun equip-item (player item)
  "Equip an item (weapon or armor). Returns (values success message)"
  (let ((item-type (mud.inventory:item-type item)))
    (cond
      ((eq item-type :weapon)
       ;; Unequip current weapon if any
       (when (player-equipped-weapon player)
         (setf (player-equipped-weapon player) nil))
       ;; Equip new weapon (item stays in inventory)
       (setf (player-equipped-weapon player) item)
       (values t (format nil "You wield ~a." (mud.inventory:item-name item))))

      ((eq item-type :armor)
       ;; Unequip current armor if any
       (when (player-equipped-armor player)
         (setf (player-equipped-armor player) nil))
       ;; Equip new armor (item stays in inventory)
       (setf (player-equipped-armor player) item)
       (values t (format nil "You wear ~a." (mud.inventory:item-name item))))

      (t
       (values nil (format nil "You can't equip ~a." (mud.inventory:item-name item)))))))

(defun unequip-item (player slot)
  "Unequip an item from a slot (:weapon or :armor). Returns (values success message)"
  (cond
    ((eq slot :weapon)
     (if (player-equipped-weapon player)
         (let ((item-name (mud.inventory:item-name (player-equipped-weapon player))))
           (declare (ignorable item-name))
           (setf (player-equipped-weapon player) nil)
           (values t (format nil "You unequip ~a." item-name)))
         (values nil "You don't have a weapon equipped.")))

    ((eq slot :armor)
     (if (player-equipped-armor player)
         (let ((item-name (mud.inventory:item-name (player-equipped-armor player))))
           (declare (ignorable item-name))
           (setf (player-equipped-armor player) nil)
           (values t (format nil "You unequip ~a." item-name)))
         (values nil "You don't have armor equipped.")))

    (t
     (values nil "Invalid slot. Use 'weapon' or 'armor'."))))

(defun add-to-inventory (player item)
  "Forward to inventory:add-to-inventory so currency stays in sync."
  (mud.inventory:add-to-inventory player item))


;; Persistence helpers for server snapshots.

(defun %serialize-item (item)
  "Serialize an item by storing its template name instead of all attributes"
  (when item
    (mud.inventory:item-name item)))

(defun %serialize-inventory-with-quantities (inventory)
  "Serialize inventory as item quantities instead of individual items"
  (when inventory
    (let ((item-counts (make-hash-table :test #'equal)))
      ;; Count each item type
      (dolist (item inventory)
        (when item
          (let ((item-name (mud.inventory:item-name item)))
            (incf (gethash item-name item-counts 0)))))
      ;; Convert to alist
      (let ((result nil))
        (maphash (lambda (name count)
                   (push (cons name count) result))
                 item-counts)
        (nreverse result)))))

(defun %deserialize-item (data)
  "Deserialize an item by creating it from the template name"
  (when data
    (if (stringp data)
        ;; New format: just the template name
        (mud.inventory:create-item data)
        ;; Old format: full item data (for backwards compatibility)
        (apply #'mud.inventory:make-item data))))

(defun %deserialize-inventory-with-quantities (data)
  "Deserialize inventory from item quantities"
  (when data
    (let ((inventory nil))
      (dolist (item-quantity data)
        (let ((item-name (car item-quantity))
              (quantity (cdr item-quantity)))
          (dotimes (i quantity)
            (let ((item (mud.inventory:create-item item-name)))
              (when item
                (push item inventory))))))
      (nreverse inventory))))

(defun %quest-state->alist (table)
  (when table
    (let ((result nil))
      (maphash (lambda (quest-id state)
                 (push (cons quest-id state) result))
               table)
      (nreverse result))))

(defun %alist->quest-state (alist)
  (when alist
    (let ((table (make-hash-table :test #'eq)))
      (dolist (entry alist table)
        (setf (gethash (car entry) table) (cdr entry))))))

(defun %serialize-inventory (player)
  (let* ((items (copy-list (player-inventory player)))
         ;; Add equipped items to inventory if they're not already there
         (all-items (let ((result items))
                      (when (and (player-equipped-weapon player)
                                 (not (member (player-equipped-weapon player) items :test #'eq)))
                        (push (player-equipped-weapon player) result))
                      (when (and (player-equipped-armor player)
                                 (not (member (player-equipped-armor player) items :test #'eq)))
                        (push (player-equipped-armor player) result))
                      result))
         (serialized (%serialize-inventory-with-quantities all-items))
         (weapon-index (and (player-equipped-weapon player)
                            (position (player-equipped-weapon player) all-items :test #'eq)))
         (armor-index (and (player-equipped-armor player)
                           (position (player-equipped-armor player) all-items :test #'eq))))
    (values serialized weapon-index armor-index)))

(defun %restore-inventory (player data weapon-index armor-index)
  (let ((items (if (and data (listp data) (consp (first data)))
                   ;; New format: item quantities
                   (%deserialize-inventory-with-quantities data)
                   ;; Old format: individual items
                   (mapcar #'%deserialize-item data))))
    (setf (player-inventory player) items)
    (when (and (integerp weapon-index) (<= 0 weapon-index) (< weapon-index (length items)))
      (setf (player-equipped-weapon player) (nth weapon-index items)))
    (when (and (integerp armor-index) (<= 0 armor-index) (< armor-index (length items)))
      (setf (player-equipped-armor player) (nth armor-index items)))
    ;; Remove equipped items from inventory to avoid duplicates
    (when (player-equipped-weapon player)
      (setf (player-inventory player) 
            (remove (player-equipped-weapon player) (player-inventory player) :test #'eq)))
    (when (player-equipped-armor player)
      (setf (player-inventory player) 
            (remove (player-equipped-armor player) (player-inventory player) :test #'eq)))))

(defun show-simple-status (player)
  "Show basic player status (used for auto-status updates)"
  (when (player-stream player)
    (let ((stream (player-stream player)))
      (format stream "~a~%" 
              (mud.ansi:wrap
               (format nil "Health: ~d/~d  Mana: ~d/~d" 
                       (player-health player) (player-max-health player)
                       (player-mana player) (player-max-mana player))
               :bright-cyan))
      (format stream "~a~%" 
              (mud.ansi:wrap
               (format nil "Damage: ~d  Armor: ~d  Gold: ~d" 
                       (get-player-damage player) (get-player-armor player)
                       (player-gold player))
               :bright-cyan))
      (force-output stream))))

(defun %serialize-player (player)
  (multiple-value-bind (inventory weapon-index armor-index)
      (%serialize-inventory player)
    (list :name (player-name player)
          :room (player-room player)
          :health (player-health player)
          :max-health (player-max-health player)
          :mana (player-mana player)
          :max-mana (player-max-mana player)
          :level (player-level player)
          :xp (player-xp player)
          :gold (player-gold player)
          :inventory inventory
          :equipped-weapon-index weapon-index
          :equipped-armor-index armor-index
          :quest-state (%quest-state->alist (player-quest-state player))
          :vehicle (player-vehicle player))))

(defun %restore-player (data default-room valid-room-p)
  (let ((name (getf data :name)))
    (when (null name)
      (return-from %restore-player nil))
    (let* ((requested-room (getf data :room))
           (initial-room (or requested-room default-room))
           (player (make-player :name name :room initial-room :stream nil :socket nil)))
      (when valid-room-p
        (let ((validated
                (cond
                  ((and requested-room (funcall valid-room-p requested-room)) requested-room)
                  ((and initial-room (funcall valid-room-p initial-room)) initial-room)
                  ((and default-room (funcall valid-room-p default-room)) default-room)
                  (t initial-room))))
          (setf (player-room player) validated)))
      (let ((max-health (getf data :max-health)))
        (when max-health
          (setf (player-max-health player) max-health)))
      (let ((health (getf data :health)))
        (when health
          (set-player-health player health)))
      (let ((max-mana (getf data :max-mana)))
        (when max-mana
          (setf (player-max-mana player) max-mana)))
      (let ((mana (getf data :mana)))
        (when mana
          (set-player-mana player mana)))
      (let ((level (getf data :level)))
        (when level
          (setf (player-level player) level)))
      (let ((xp (getf data :xp)))
        (when xp
          (setf (player-xp player) xp)))
      (let ((gold (getf data :gold)))
        (when gold
          (setf (player-gold player) gold)))
      (%restore-inventory player
                          (or (getf data :inventory) '())
                          (getf data :equipped-weapon-index)
                          (getf data :equipped-armor-index))
      (let ((quest (getf data :quest-state)))
        (setf (player-quest-state player) (%alist->quest-state quest)))
      (setf (player-vehicle player) (getf data :vehicle))
      player)))

(defun collect-player-snapshots ()
  (sb-thread:with-mutex (*player-registry-lock*)
    (let ((snapshots (loop for player being the hash-values of *player-registry*
                           collect (%serialize-player player))))
      (sort snapshots #'string-lessp :key (lambda (entry) (string (getf entry :name "")))))))

(defun restore-player-snapshots (snapshots &key default-room valid-room-p)
  (sb-thread:with-mutex (*player-registry-lock*)
    (clrhash *player-registry*)
    (let ((count 0))
  (dolist (entry snapshots count)
    (handler-case
            (let ((player (%restore-player entry default-room valid-room-p)))
      (when player
        (setf (gethash (%normalize-player-key (player-name player)) *player-registry*)
          player)
        (incf count)))
      (error (err)
    (warn "Failed to restore player snapshot for ~a: ~a"
      (getf entry :name) err))))
  count)))
