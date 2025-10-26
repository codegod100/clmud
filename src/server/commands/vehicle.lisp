(in-package :mud.server)

(define-command (("enter") command-enter) (player rest)
  (if (zerop (length rest))
      (write-crlf (player-stream player)
       (wrap "Enter what?" :bright-red))
      (let* ((target-name (string-trim '(#\  #\Tab) rest))
             (vehicle-item (find-item-in-room (player-room player) target-name)))
        (cond
          ((player-vehicle player)
           (write-crlf (player-stream player)
            (wrap
             (format nil "You are already in ~a."
                     (item-name (player-vehicle player)))
             :bright-red)))
          ((or (null vehicle-item)
               (not (eq (item-type vehicle-item) :vehicle)))
           (write-crlf (player-stream player)
            (wrap (format nil "You can't enter '~a'." target-name)
             :bright-red)))
          (t
           (remove-item-from-room (player-room player) vehicle-item)
           (setf (player-vehicle player) vehicle-item)
           (write-crlf (player-stream player)
            (wrap (format nil "You enter ~a." (item-name vehicle-item))
             :bright-cyan))
           (announce-to-room player
            (format nil "~a enters ~a."
                    (wrap (player-name player) :bright-blue)
                    (item-name vehicle-item))
            :include-self nil)
           (send-room-overview player))))))

(define-command (("exit") command-exit-vehicle) (player rest)
  (declare (ignore rest))
  (cond
    ((null (player-vehicle player))
     (write-crlf (player-stream player)
      (wrap "You are not in a vehicle." :bright-red)))
    (t
     (let ((vehicle-item (player-vehicle player)))
       (add-item-to-room (player-room player) vehicle-item)
       (setf (player-vehicle player) nil)
       (write-crlf (player-stream player)
        (wrap (format nil "You exit ~a." (item-name vehicle-item))
         :bright-cyan))
       (announce-to-room player
        (format nil "~a exits ~a."
                (wrap (player-name player) :bright-blue)
                (item-name vehicle-item))
        :include-self nil)
       (send-room-overview player)))))

(define-command (("uber") command-uber) (player rest)
  (cond
    ((null (player-vehicle player))
     (write-crlf (player-stream player)
      (wrap "You need to be in a vehicle to use uber." :bright-red)))
    ((not (eq (mud.inventory:item-vehicle-type (player-vehicle player)) :uber))
     (write-crlf (player-stream player)
      (wrap "This vehicle doesn't support uber travel." :bright-red)))
    ((zerop (length rest))
     (write-crlf (player-stream player)
      (wrap "Uber to where? Usage: uber <location name>" :bright-red)))
    (t
     (let* ((destination-name (string-trim '(#\  #\Tab) rest))
            (destination-room (find-room-by-name destination-name)))
       (if destination-room
           (progn
             (write-crlf (player-stream player)
              (wrap
               (format nil
                       "The ~a shimmers with energy and instantly transports you to ~a!"
                       (item-name (player-vehicle player))
                       (mud.world:room-name destination-room))
               :bright-magenta))
             (announce-to-room player
              (format nil "~a vanishes in a flash of light!"
                      (wrap (player-name player) :bright-blue))
              :include-self nil)
             (set-player-room player (mud.world:room-id destination-room))
             (send-room-overview player)
             (announce-to-room player
              (format nil
                      "~a appears in a flash of light, riding in ~a!"
                      (wrap (player-name player) :bright-green)
                      (item-name (player-vehicle player)))
              :include-self nil))
           (write-crlf (player-stream player)
            (wrap
             (format nil "Location '~a' not found." destination-name)
             :bright-red)))))))
