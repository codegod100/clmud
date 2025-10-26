(in-package :mud.server)


(defun parse-command (line)
  (let* ((trimmed (string-trim *whitespace-chars* line)))
    (if (zerop (length trimmed))
        (values nil "")
        (let ((split (position-if #'whitespace-char-p trimmed)))
          (if split
              (values (string-downcase (subseq trimmed 0 split))
                      (string-left-trim *whitespace-chars*
                                        (subseq trimmed split)))
              (values (string-downcase trimmed) ""))))))


(defun normalize-direction (dir-string)
  "Convert direction string to keyword, handling single-letter aliases"
  (let ((dir (string-downcase dir-string)))
    (cond ((string= dir "n") :north) ((string= dir "s") :south)
          ((string= dir "e") :east) ((string= dir "w") :west)
          ((string= dir "u") :up) ((string= dir "d") :down)
          ((string= dir "ne") :northeast) ((string= dir "nw") :northwest)
          ((string= dir "se") :southeast) ((string= dir "sw") :southwest)
          (t (intern (string-upcase dir) :keyword)))))


(defparameter *command-dispatch* (make-hash-table :test #'equal))

(defun register-command-handler (names handler)
  (dolist (name names)
    (setf (gethash (string-downcase name) *command-dispatch*) handler)))

(defmacro define-command ((names function-name) (player rest) &body body)
  `(progn
     (defun ,function-name (,player ,rest)
       ,@body)
     (register-command-handler ',names #',function-name)))

(defun register-direction-command (name)
  (let ((direction (normalize-direction name)))
    (register-command-handler (list name)
      (lambda (player rest)
        (declare (ignore rest))
        (move-player player direction)))))

(eval-when (:load-toplevel :execute)
  (dolist (alias '("n" "s" "e" "w" "u" "d" "ne" "nw" "se" "sw" "downstream" "upstream"))
    (register-direction-command alias)))

(defun maybe-announce-quest-rewards (player)
  (multiple-value-bind (completed leveled-up quest)
      (check-quest-completion player :apple-picking)
    (when completed
      (let ((stream (player-stream player)))
        (write-crlf stream
         (wrap (format nil "Quest Complete: ~a" (quest-name quest))
          :bright-yellow))
        (write-crlf stream (wrap (quest-reward-text quest) :bright-green))
        (write-crlf stream
         (wrap (format nil "You gained ~d XP!" (quest-reward-xp quest))
          :bright-cyan))
        (when leveled-up
          (write-crlf stream
           (wrap
            (format nil
                    "*** LEVEL UP! You are now level ~d! ***"
                    (player-level player))
            :bright-magenta))
          (write-crlf stream
           (wrap
            (format nil
                    "Health: +10 (now ~d)  Mana: +5 (now ~d)"
                    (player-max-health player)
                    (player-max-mana player))
            :bright-green)))))))

(defun item-slot-score (item slot)
  (cond
    ((null item) 0)
    ((eq slot :weapon) (item-damage item))
    ((eq slot :armor) (item-armor item))
    (t 0)))

(defun best-inventory-item-for-slot (player slot)
  (let* ((inventory (player-inventory player))
         (candidates (remove-if-not (lambda (item)
                                      (eq (item-slot item) slot))
                                    inventory)))
    (when candidates
      (reduce (lambda (best candidate)
                (if (> (item-slot-score candidate slot)
                       (item-slot-score best slot))
                    candidate
                    best))
              (rest candidates)
              :initial-value (first candidates)))))

(defun handle-equip-all (player)
  (let ((equipped 0)
        (stream (player-stream player)))
    (labels ((equip-slot (slot accessor)
               (let* ((candidate (best-inventory-item-for-slot player slot))
                      (current (funcall accessor player))
                      (candidate-score (item-slot-score candidate slot))
                      (current-score (item-slot-score current slot)))
                 (when (and candidate
                            (or (null current)
                                (> candidate-score current-score)))
                   (remove-from-inventory player candidate)
                   (multiple-value-bind (success message)
                       (equip-item player candidate)
                     (if success
                         (progn
                           (incf equipped)
                           (write-crlf stream (wrap message :bright-green))
                           (announce-to-room player
                            (format nil "~a equips ~a."
                                    (wrap (player-name player)
                                     :bright-yellow)
                                    (item-name candidate))
                            :include-self nil))
                         (progn
                           (add-to-inventory player candidate)
                           (write-crlf stream (wrap message :bright-red)))))))))
      (equip-slot :weapon #'player-equipped-weapon)
      (equip-slot :armor #'player-equipped-armor)
      (when (zerop equipped)
        (write-crlf stream
         (wrap "You have nothing better to equip." :bright-red))))))

(defun handle-get-all (player)
  (let* ((room-id (player-room player))
         (room (find-room room-id))
         (stream (player-stream player))
         (items (when room (copy-list (mud.world::room-items room))))
         (collected 0))
    (cond
      ((null room)
       (write-crlf stream
        (wrap "You are nowhere. There's nothing to take." :bright-red)))
      ((null items)
       (write-crlf stream (wrap "There is nothing here to take." :bright-red)))
      (t
       (dolist (item items)
         (let ((target-name (item-name item)))
           (if (eq (item-type item) :corpse)
               (let ((corpse-items (loot-corpse item)))
                 (if corpse-items
                     (progn
                       (dolist (corpse-item corpse-items)
                         (add-to-inventory player corpse-item)
                         (incf collected)
                         (maybe-announce-quest-rewards player))
                       (remove-item-from-room room-id item)
                       (write-crlf stream
                        (wrap
                         (format nil
                                 "You loot the corpse and take ~d item~:p."
                                 (length corpse-items))
                         :bright-green))
                       (announce-to-room player
                        (format nil "~a loots ~a."
                                (wrap (player-name player)
                                 :bright-yellow)
                                target-name)
                        :include-self nil))
                     (write-crlf stream
                      (wrap "The corpse is empty." :bright-red))))
               (multiple-value-bind (success message)
                   (grab-item player target-name)
                 (if success
                     (progn
                       (incf collected)
                       (write-crlf stream (wrap message :bright-green))
                       (announce-to-room player
                        (format nil "~a gets ~a."
                                (wrap (player-name player)
                                 :bright-yellow)
                                target-name)
                        :include-self nil)
                       (maybe-announce-quest-rewards player))
                     (write-crlf stream (wrap message :bright-red)))))))
       (when (zerop collected)
         (write-crlf stream
          (wrap "You fail to pick up anything." :bright-red)))))))

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

(define-command (("look" "l") command-look) (player rest)
  (if (zerop (length rest))
      (send-room-overview player)
      (handle-look-at player rest)))

(define-command (("move" "go") command-move) (player rest)
  (if (zerop (length rest))
      (write-crlf (player-stream player)
       (wrap "Go where?" :bright-red))
      (let* ((dir-token
               (subseq rest 0
                       (or (position-if #'whitespace-char-p rest)
                           (length rest))))
             (keyword (normalize-direction dir-token)))
        (move-player player keyword))))

(define-command (("say") command-say) (player rest)
  (handle-say player rest))

(define-command (("who") command-who) (player rest)
  (declare (ignore rest))
  (with-mutex (*clients-lock*)
    (let ((names (mapcar #'player-name *clients*)))
      (write-crlf (player-stream player)
       (wrap
        (if names
            (format nil "Wanderers about: ~{~a~^, ~}" names)
            "You are alone in the twilight.")
        :bright-magenta)))))

(define-command (("stats") command-stats) (player rest)
  (declare (ignore rest))
  (write-crlf (player-stream player)
   (wrap (get-player-stats player) :bright-cyan)))

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

(define-command (("attack") command-attack) (player rest)
  (if (zerop (length rest))
      (write-crlf (player-stream player)
       (wrap "Attack what? Usage: attack <target>" :bright-red))
      (let* ((target-name (string-trim '(#\  #\Tab) rest))
             (mob (find-mob-in-room (player-room player) target-name)))
        (if mob
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
              (resolve-mob-hit player mob actual-damage))
            (write-crlf (player-stream player)
             (wrap
              (format nil "There is no ~a here to attack."
                      target-name)
              :bright-red))))))

(define-command (("ram") command-ram) (player rest)
  (handle-ram player rest))

(define-command (("equip") command-equip) (player rest)
  (let ((item-name (string-trim '(#\  #\Tab) rest)))
    (cond
      ((zerop (length item-name))
       (write-crlf (player-stream player)
        (wrap "Equip what? Usage: equip <item>" :bright-red)))
      ((string-equal item-name "all")
       (handle-equip-all player))
      (t
       (let ((item (find-in-inventory player item-name)))
         (if item
             (progn
               (remove-from-inventory player item)
               (multiple-value-bind (success message)
                   (equip-item player item)
                 (write-crlf (player-stream player)
                  (wrap message (if success :bright-green :bright-red)))
                 (if success
                     (announce-to-room player
                      (format nil "~a equips ~a."
                              (wrap (player-name player) :bright-yellow)
                              item-name)
                      :include-self nil)
                     (add-to-inventory player item))))
             (write-crlf (player-stream player)
              (wrap (format nil "You don't have any ~a." item-name)
               :bright-red))))))))

(define-command (("unequip") command-unequip) (player rest)
  (if (zerop (length rest))
      (write-crlf (player-stream player)
       (wrap "Unequip what? Usage: unequip weapon/armor" :bright-red))
      (let* ((slot-name (string-trim '(#\  #\Tab) rest))
             (slot (cond ((or (string-equal slot-name "weapon")
                              (string-equal slot-name "w"))
                          :weapon)
                         ((or (string-equal slot-name "armor")
                              (string-equal slot-name "a"))
                          :armor)
                         (t nil))))
        (if slot
            (multiple-value-bind (success message)
                (unequip-item player slot)
              (write-crlf (player-stream player)
               (wrap message (if success :bright-green :bright-red)))
              (when success
                (announce-to-room player
                 (format nil "~a unequips ~a."
                         (wrap (player-name player) :bright-yellow)
                         slot-name)
                 :include-self nil)))
            (write-crlf (player-stream player)
             (wrap
              "Unequip weapon or armor? Usage: unequip weapon/armor"
              :bright-red))))))

(define-command (("inventory" "inv" "i") command-inventory) (player rest)
  (declare (ignore rest))
  (write-crlf (player-stream player) (list-inventory player)))

(define-command (("use") command-use) (player rest)
  (if (zerop (length rest))
      (write-crlf (player-stream player)
       (wrap "Use what? Usage: use <item>" :bright-red))
      (let ((item-name (string-trim '(#\  #\Tab) rest)))
        (multiple-value-bind (success message)
            (use-item player item-name)
          (if success
              (write-crlf (player-stream player)
               (wrap message :bright-green))
              (write-crlf (player-stream player)
               (wrap message :bright-red)))))))

(define-command (("drop") command-drop) (player rest)
  (if (zerop (length rest))
      (write-crlf (player-stream player)
       (wrap "Drop what? Usage: drop <item>" :bright-red))
      (let ((item-name (string-trim '(#\  #\Tab) rest)))
        (multiple-value-bind (success message)
            (drop-item player item-name)
          (if success
              (progn
                (write-crlf (player-stream player)
                 (wrap message :bright-green))
                (announce-to-room player
                 (format nil "~a drops ~a."
                         (wrap (player-name player) :bright-yellow)
                         item-name)
                 :include-self nil))
              (write-crlf (player-stream player)
               (wrap message :bright-red)))))))

(define-command (("get" "grab") command-get) (player rest)
  (let ((item-name (string-trim '(#\  #\Tab) rest)))
    (cond
      ((zerop (length item-name))
       (write-crlf (player-stream player)
        (wrap "Get what? Usage: get <item>" :bright-red)))
      ((string-equal item-name "all")
       (handle-get-all player))
      (t
       (let ((item (find-item-in-room (player-room player) item-name)))
         (if (and item (eq (item-type item) :corpse))
             (let ((corpse-items (loot-corpse item)))
               (if corpse-items
                   (progn
                     (dolist (corpse-item corpse-items)
                       (add-to-inventory player corpse-item)
                       (maybe-announce-quest-rewards player))
                     (remove-item-from-room (player-room player) item)
                     (write-crlf (player-stream player)
                      (wrap
                       (format nil
                               "You loot the corpse and take ~d item~:p."
                               (length corpse-items))
                       :bright-green))
                     (announce-to-room player
                      (format nil "~a loots ~a."
                              (wrap (player-name player) :bright-yellow)
                              item-name)
                      :include-self nil))
                   (write-crlf (player-stream player)
                    (wrap "The corpse is empty." :bright-red))))
             (multiple-value-bind (success message)
                 (grab-item player item-name)
               (if success
                   (progn
                     (write-crlf (player-stream player)
                      (wrap message :bright-green))
                     (announce-to-room player
                      (format nil "~a gets ~a."
                              (wrap (player-name player) :bright-yellow)
                              item-name)
                      :include-self nil)
                     (maybe-announce-quest-rewards player))
                   (write-crlf (player-stream player)
                    (wrap message :bright-red))))))))))

(define-command ((".") command-repeat) (player rest)
  (declare (ignore rest))
  (write-crlf (player-stream player)
   (wrap "No previous command to repeat." :bright-red)))

(define-command (("suicide") command-suicide) (player rest)
  (declare (ignore rest))
  (write-crlf (player-stream player)
   (wrap "You take your own life..." :bright-red))
  (announce-to-room player
   (format nil "~a falls to the ground, lifeless."
           (wrap (player-name player) :bright-red))
   :include-self nil)
  (handle-player-death player)
  (write-crlf (player-stream player)
   (wrap "You have died! Your items have been left in a corpse."
    :bright-red))
  (write-crlf (player-stream player)
   (wrap "You awaken in the graveyard, wounded but alive..."
    :bright-black))
  (send-room-overview player)
  (announce-to-room player
   (format nil "~a appears, looking worse for wear."
           (wrap (player-name player) :bright-green))
   :include-self nil))

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

(define-command (("status") command-status) (player rest)
  (declare (ignore rest))
  (write-crlf (player-stream player)
   (wrap
    (format nil "~a - Level ~d" (player-name player)
            (player-level player))
    :bright-cyan))
  (write-crlf (player-stream player)
   (format nil "Health: ~d/~d  Mana: ~d/~d" (player-health player)
           (player-max-health player) (player-mana player)
           (player-max-mana player)))
  (write-crlf (player-stream player)
   (format nil "XP: ~d/~d  (Need ~d more to level)" (player-xp player)
           (mud.player:xp-for-level (+ (player-level player) 1))
           (xp-to-next-level player)))
  (write-crlf (player-stream player)
   (format nil "Damage: ~d  Armor: ~d" (get-player-damage player)
           (get-player-armor player)))
  (when (player-equipped-weapon player)
    (write-crlf (player-stream player)
     (format nil "Weapon: ~a (~+d damage)"
             (item-name (player-equipped-weapon player))
             (item-damage (player-equipped-weapon player)))))
  (when (player-equipped-armor player)
    (write-crlf (player-stream player)
     (format nil "Armor: ~a (~+d armor)"
             (item-name (player-equipped-armor player))
             (item-armor (player-equipped-armor player))))))

(define-command (("quest") command-quest) (player rest)
  (if (zerop (length rest))
      (let ((active-quests (get-active-quests player)))
        (if active-quests
            (progn
              (write-crlf (player-stream player)
               (wrap "Active Quests:" :bright-yellow))
              (dolist (quest active-quests)
                (write-crlf (player-stream player)
                 (format nil "  ~a: ~a"
                         (wrap (quest-name quest) :bright-cyan)
                         (quest-description quest)))))
            (write-crlf (player-stream player)
             (wrap
              "You have no active quests. Try 'quest start apple' to begin your first quest!"
              :bright-yellow))))
      (let ((subcommand (string-trim '(#\  #\Tab) rest)))
        (cond
          ((string= subcommand "start apple")
           (let ((result (start-quest player :apple-picking)))
             (write-crlf (player-stream player)
              (wrap result :bright-green))))
          (t
           (write-crlf (player-stream player)
            (wrap "Usage: quest [start apple]" :bright-red)))))))

(define-command (("help") command-help) (player rest)
  (declare (ignore rest))
  (write-crlf (player-stream player)
   (wrap "Commands:" :bright-yellow))
  (write-crlf (player-stream player)
   "  Movement: look (l), go <dir> (n/s/e/w/u/d/ne/nw/se/sw), enter <vehicle>, exit, uber <location>")
  (write-crlf (player-stream player) "  Social: say <text>, who")
  (write-crlf (player-stream player)
   "  Combat: attack <mob>, cast <spell> <target>, stats, spells")
  (write-crlf (player-stream player)
   "  Equipment: equip <item>, unequip weapon/armor")
  (write-crlf (player-stream player)
   "  Inventory: inventory (inv/i), use <item>, drop <item>, get <item> (loot corpses)")
  (write-crlf (player-stream player)
   "  Quests: quest, quest start apple, status")
  (write-crlf (player-stream player)
   "  Other: help, quit, . (repeat last command), suicide (test death)"))

(define-command (("quit") command-quit) (player rest)
  (declare (ignore rest))
  :quit)

(defun handle-command (player line)
  (multiple-value-bind (verb rest)
      (parse-command line)
    (cond
     ((null verb) (send-room-overview player))
     (t
      (let ((handler (gethash verb *command-dispatch*)))
        (if handler
            (funcall handler player rest)
            (write-crlf (player-stream player)
             (wrap "Unknown command." :bright-red))))))))
