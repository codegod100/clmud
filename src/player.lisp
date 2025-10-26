(in-package :mud.player)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (declaim (ftype (function (t) integer) mud.inventory:item-damage)
           (ftype (function (t) integer) mud.inventory:item-armor)
           (ftype (function (t) keyword) mud.inventory:item-type)
           (ftype (function (t) keyword) mud.inventory:item-slot)
           (ftype (function (t) string) mud.inventory:item-name)
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
  (set-player-health player (+ (player-health player) delta)))

(defun modify-mana (player delta)
  "Add or subtract mana from player, clamping to 0..max-mana"
  (set-player-mana player (+ (player-mana player) delta)))

(defun modify-gold (player delta)
  "Adjust player gold, never letting it drop below zero."
  (set-player-gold player (+ (player-gold player) delta)))

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
