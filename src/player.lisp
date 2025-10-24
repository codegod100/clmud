(in-package :mud.player)

(defclass player ()
  ((name :initarg :name :accessor player-name)
   (room :initarg :room :accessor player-room)
   (stream :initarg :stream :accessor player-stream)
   (socket :initarg :socket :accessor player-socket))
  (:documentation "Represents a connected adventurer."))

(defun make-player (&key name room stream socket)
  (make-instance 'player :name name :room room :stream stream :socket socket))

(defun set-player-room (player new-room)
  (setf (player-room player) new-room))
