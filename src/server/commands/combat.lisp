(in-package :mud.server)

(defun handle-ram (player target-input)
  (let ((vehicle (player-vehicle player))
        (stream (player-stream player))
        (target-name (string-trim '(#\  #\Tab) target-input)))
    (cond
      ((null vehicle)
       (write-crlf stream
        (wrap "You need to be in a vehicle to ram." :bright-red)))
      ((zerop (length target-name))
       (write-crlf stream
        (wrap "Ram what? Usage: ram <target>" :bright-red)))
      (t
       (let ((mob (find-mob-in-room (player-room player) target-name)))
         (if (null mob)
             (write-crlf stream
              (wrap (format nil "There is no ~a here to ram." target-name)
               :bright-red))
             (let* ((vehicle-template
                      (mud.world::find-vehicle (item-name vehicle)))
                    (vehicle-damage
                      (if vehicle-template
                          (mud.world::vehicle-damage vehicle-template)
                          0))
                    (vehicle-speed
                      (if vehicle-template
                          (mud.world::vehicle-speed vehicle-template)
                          0))
                    (base-damage (get-player-damage player))
                    (speed-bonus (floor vehicle-speed 2))
                    (total-damage (max 1 (+ base-damage vehicle-damage
                                             speed-bonus))))
               (write-crlf stream
                (wrap
                 (format nil "You ram ~a with ~a for ~d damage!"
                         (mob-name mob)
                         (item-name vehicle)
                         total-damage)
                 :bright-red))
               (announce-to-room player
                (format nil "~a rams ~a with ~a!"
                        (wrap (player-name player) :bright-yellow)
                        (mob-name mob)
                        (wrap (item-name vehicle) :bright-blue))
                :include-self nil)
               (resolve-mob-hit player mob total-damage))))))))

(define-command (("attack") command-attack) (player rest)
  (if (zerop (length rest))
      (write-crlf (player-stream player)
       (wrap "Attack what? Usage: attack <target>" :bright-red))
      (let* ((target-name (string-trim '(#\  #\Tab) rest))
             (mob (find-mob-in-room (player-room player) target-name)))
        (if mob
            (progn
              ;; Start automatic combat
              (mud.mob::start-combat mob player)
              (write-crlf (player-stream player)
               (wrap
                (format nil "You engage ~a in combat!" (mob-name mob))
                :bright-yellow))
              (announce-to-room player
               (format nil "~a engages ~a in combat!"
                       (wrap (player-name player) :bright-yellow)
                       (mob-name mob))
               :include-self nil)
              ;; Do initial attack
              (let* ((player-damage (get-player-damage player))
                     (mob-armor (mob-armor mob))
                     (actual-damage (max 1 (- player-damage mob-armor))))
                (write-crlf (player-stream player)
                 (wrap
                  (format nil "You attack ~a for ~d damage!"
                          (mob-name mob) actual-damage)
                  :bright-red))
                (announce-to-room player
                 (format nil "~a attacks ~a!"
                         (wrap (player-name player) :bright-yellow)
                         (mob-name mob))
                 :include-self nil)
                (resolve-mob-hit player mob actual-damage)))
            (write-crlf (player-stream player)
             (wrap
              (format nil "There is no ~a here to attack."
                      target-name)
              :bright-red))))))

(define-command (("ram") command-ram) (player rest)
  (handle-ram player rest))

(define-command (("spells") command-spells) (player rest)
  (declare (ignore rest))
  (write-crlf (player-stream player)
   (wrap "Available spells:" :bright-yellow))
  (dolist (spell *spells*)
    (write-crlf (player-stream player)
     (format nil "  ~a (~d mana, ~a dmg) - ~a"
             (wrap (spell-name spell) :bright-magenta)
             (spell-cost spell)
             (if (minusp (spell-damage spell))
                 (format nil "heals ~d" (abs (spell-damage spell)))
                 (spell-damage spell))
             (spell-description spell)))))

(define-command (("cast") command-cast) (player rest)
  (handle-cast player rest))

(define-command (("flee") command-flee) (player rest)
  (declare (ignore rest))
  (let ((room-id (player-room player))
        (mobs (mud.mob::get-mobs-in-room room-id)))
    (let ((combat-mobs (remove-if-not #'mud.mob::mob-in-combat-p mobs)))
      (if combat-mobs
          (progn
            ;; End combat with all mobs in the room
            (dolist (mob combat-mobs)
              (when (eq (mud.mob::mob-combat-target mob) player)
                (mud.mob::end-combat mob)))
            (write-crlf (player-stream player)
             (wrap "You flee from combat!" :bright-yellow))
            (announce-to-room player
             (format nil "~a flees from combat!"
                     (wrap (player-name player) :bright-yellow))
             :include-self nil))
          (write-crlf (player-stream player)
           (wrap "You are not in combat." :bright-red))))))
