(in-package :mud.player)

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
   (quest-state :initarg :quest-state :accessor player-quest-state :initform nil))
  (:documentation "Represents a connected adventurer."))

(defun make-player (&key name room stream socket)
  (make-instance 'player :name name :room room :stream stream :socket socket
                         :health 100 :max-health 100
                         :mana 50 :max-mana 50
                         :level 1
                         :xp 0
                         :inventory nil
                         :quest-state nil))

(defun set-player-room (player new-room)
  (setf (player-room player) new-room))

(defun set-player-health (player value)
  (setf (player-health player) (max 0 (min value (player-max-health player)))))

(defun set-player-mana (player value)
  (setf (player-mana player) (max 0 (min value (player-max-mana player)))))

(defun modify-health (player delta)
  "Add or subtract health from player, clamping to 0..max-health"
  (set-player-health player (+ (player-health player) delta)))

(defun modify-mana (player delta)
  "Add or subtract mana from player, clamping to 0..max-mana"
  (set-player-mana player (+ (player-mana player) delta)))

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
