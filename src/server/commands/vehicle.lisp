(in-package :mud.server)

;; Enter command removed - use 'use <vehicle>' instead

(defun is-sky-room-p (room-id)
  "Check if a room is a sky room by looking for 'sky-over' in the name"
  (let ((room-name (symbol-name room-id)))
    (search "SKY-OVER" (string-upcase room-name))))

(defun find-ground-room-from-sky (sky-room-id)
  "Find the corresponding ground room from a sky room ID"
  (let ((sky-name (symbol-name sky-room-id)))
    (when (search "SKY-OVER-" (string-upcase sky-name))
      (let ((ground-name (subseq sky-name 9))) ; Remove "SKY-OVER-" prefix
        ;; Map sky room names to actual ground room names
        (case (intern (string-upcase ground-name) :keyword)
          (:village (intern "VILLAGE-SQUARE" :mud.world))
          (:forest (intern "WHISPERING-WOOD" :mud.world))
          (:market (intern "MARKET-STALLS" :mud.world))
          (:river (intern "RIVERBANK" :mud.world))
          (:graveyard (intern "GRAVEYARD" :mud.world))
          (t (intern (string-upcase ground-name) :mud.world)))))))

(defun calculate-fall-damage (player)
  "Calculate fall damage based on player level and current health"
  (let ((base-damage 20)
        (level-factor (max 1 (floor (/ (player-level player) 2))))
        (health-factor (max 1 (floor (/ (player-health player) 10)))))
    (+ base-damage level-factor health-factor)))

(define-command (("exit") command-exit-vehicle) (player rest)
  (declare (ignore rest))
  (cond
    ((null (player-vehicle player))
     (write-crlf (player-stream player)
      (wrap "You are not in a vehicle." :bright-red)))
    (t
     (let* ((vehicle-item (player-vehicle player))
            (current-room-id (player-room player))
            (vehicle-type (mud.inventory:item-vehicle-type vehicle-item)))
       ;; Check if exiting an air vehicle in a sky room
       (if (and (eq vehicle-type :air)
                (is-sky-room-p current-room-id))
           ;; Handle fall damage
           (let ((ground-room-id (find-ground-room-from-sky current-room-id))
                 (fall-damage (calculate-fall-damage player)))
             (if ground-room-id
                 (progn
                   ;; Announce exit from vehicle
                   (announce-to-room player
                    (format nil "~a exits ~a and plummets toward the ground!"
                            (wrap (player-name player) :bright-blue)
                            (mud.inventory:item-name vehicle-item))
                    :include-self nil)
                   ;; Remove player from vehicle
                   (setf (player-vehicle player) nil)
                   ;; Move player to ground room
                   (set-player-room player ground-room-id)
                   ;; Move vehicle to ground room (it crashes too)
                   (add-item-to-room ground-room-id vehicle-item)
                   ;; Apply fall damage
                   (mud.player:modify-health player (- fall-damage))
                   ;; Send messages
                   (write-crlf (player-stream player)
                    (wrap (format nil "You exit ~a and plummet toward the ground!" 
                            (mud.inventory:item-name vehicle-item)) :bright-red))
                   (write-crlf (player-stream player)
                    (wrap (format nil "You crash to the ground, taking ~d fall damage!" 
                            fall-damage) :bright-red))
                   (write-crlf (player-stream player)
                    (wrap (format nil "The ~a crashes to the ground nearby!" 
                            (mud.inventory:item-name vehicle-item)) :bright-red))
                   ;; Check if player died
                   (if (not (mud.player:player-alive-p player))
                       (progn
                         (write-crlf (player-stream player)
                          (wrap "The impact kills you!" :bright-red))
                         (mud.combat:handle-player-death player))
                       (progn
                         (write-crlf (player-stream player)
                          (wrap (format nil "You have ~d/~d health remaining." 
                                  (player-health player) (player-max-health player)) 
                                :bright-yellow))
                         (send-room-overview player)
                         (announce-to-room player
                          (format nil "~a crashes to the ground from above!"
                                  (wrap (player-name player) :bright-red))
                          :include-self nil)
                         (announce-to-room player
                          (format nil "The ~a crashes to the ground nearby!"
                                  (mud.inventory:item-name vehicle-item))
                          :include-self nil))))
                 ;; If no ground room found, just exit normally
                 (progn
                   (add-item-to-room current-room-id vehicle-item)
                   (setf (player-vehicle player) nil)
                   (write-crlf (player-stream player)
                    (wrap (format nil "You exit ~a." (mud.inventory:item-name vehicle-item))
                     :bright-cyan))
                   (announce-to-room player
                    (format nil "~a exits ~a."
                            (wrap (player-name player) :bright-blue)
                            (mud.inventory:item-name vehicle-item))
                    :include-self nil)
                   (send-room-overview player))))
           ;; Normal exit for non-air vehicles or non-sky rooms
           (progn
             (add-item-to-room current-room-id vehicle-item)
             (setf (player-vehicle player) nil)
             (write-crlf (player-stream player)
              (wrap (format nil "You exit ~a." (mud.inventory:item-name vehicle-item))
               :bright-cyan))
             (announce-to-room player
              (format nil "~a exits ~a."
                      (wrap (player-name player) :bright-blue)
                      (mud.inventory:item-name vehicle-item))
              :include-self nil)
             (send-room-overview player)))))))

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
                       (mud.inventory:item-name (player-vehicle player))
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
                      (mud.inventory:item-name (player-vehicle player)))
              :include-self nil))
           (write-crlf (player-stream player)
            (wrap
             (format nil "Location '~a' not found." destination-name)
             :bright-red)))))))

(define-command (("repair") command-repair) (player rest)
  (declare (ignore rest))
  (cond
    ((null (player-vehicle player))
     (write-crlf (player-stream player)
      (wrap "You need to be in a vehicle to repair it." :bright-red)))
    ((not (mud.player:vehicle-broken-p player))
     (write-crlf (player-stream player)
      (wrap "Your vehicle is already in perfect condition." :bright-yellow)))
    (t
     (let ((repair-kit (mud.inventory:find-in-inventory player "repair-kit")))
       (cond
         ((null repair-kit)
          (write-crlf (player-stream player)
           (wrap "You need a repair kit to fix your vehicle." :bright-red)))
         (t
          (multiple-value-bind (success message)
              (mud.inventory:use-item player "repair-kit")
            (write-crlf (player-stream player)
             (wrap message (if success :bright-green :bright-red))))))))))
