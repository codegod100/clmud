(in-package :mud.server)

;; Register direction commands
(eval-when (:load-toplevel :execute)
  (dolist (alias '("n" "s" "e" "w" "u" "d" "ne" "nw" "se" "sw"))
    (register-direction-command alias)))

(define-command (("look" "l") command-look) (player rest)
  (if (zerop (length rest))
      (send-room-overview player)
      (handle-look-at player rest)))

(define-command (("map") command-map) (player rest)
  (declare (ignore rest))
  (let* ((room-id (player-room player))
         (room (find-room room-id))
         (vehicle (player-vehicle player))
         (vehicle-type (when vehicle
                         (mud.inventory:item-vehicle-type vehicle))))
    (if room
        (let ((stream (player-stream player)))
          (write-crlf stream
           (wrap (generate-artistic-map room-id vehicle-type) :bright-cyan)))
        (write-crlf (player-stream player)
         (wrap "You are nowhere." :bright-red)))))
