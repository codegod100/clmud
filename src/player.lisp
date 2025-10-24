(in-package :mud.player)

(defclass player ()
  ((name :initarg :name :accessor player-name)
   (room :initarg :room :accessor player-room)
   (stream :initarg :stream :accessor player-stream)
   (socket :initarg :socket :accessor player-socket)
   (health :initarg :health :accessor player-health :initform 100)
   (max-health :initarg :max-health :accessor player-max-health :initform 100)
   (mana :initarg :mana :accessor player-mana :initform 50)
   (max-mana :initarg :max-mana :accessor player-max-mana :initform 50)
   (level :initarg :level :accessor player-level :initform 1)
   (inventory :initarg :inventory :accessor player-inventory :initform nil))
  (:documentation "Represents a connected adventurer."))

(defun make-player (&key name room stream socket)
  (make-instance 'player :name name :room room :stream stream :socket socket
                         :health 100 :max-health 100
                         :mana 50 :max-mana 50
                         :level 1
                         :inventory nil))

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
