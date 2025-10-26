(in-package :mud.server)

(defun force-save-game (player)
  "Force save the current player's state to disk"
  (handler-case
      (let ((path (merge-pathnames #P"data/save-state.lisp" *default-pathname-defaults*)))
        (ensure-directories-exist path)
        (with-open-file (out path :direction :output :if-exists :supersede
                              :if-does-not-exist :create)
          (let ((*print-readably* t)
                (*print-escape* t)
                (*package* (find-package :cl)))
            (format out ";; Saved at ~a (manual)~%" (get-universal-time))
            ;; Use the proper serialization function to include inventory
            (let ((player-data (mud.player:%serialize-player player)))
              (write (list player-data) :stream out :circle nil)
              (terpri out)
              (finish-output out)
              1))))
    (error (err)
      (format t "Save error: ~a~%" err)
      nil)))

(define-command (("stats") command-stats) (player rest)
  (declare (ignore rest))
  (write-crlf (player-stream player)
   (wrap (get-player-stats player) :bright-cyan)))

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
   (format nil "Damage: ~d  Armor: ~d  Gold: ~d" (get-player-damage player)
     (get-player-armor player)
     (player-gold player)))
  (when (player-equipped-weapon player)
    (write-crlf (player-stream player)
     (format nil "Weapon: ~a (~+d damage)"
             (item-name (player-equipped-weapon player))
             (item-damage (player-equipped-weapon player)))))
  (when (player-equipped-armor player)
    (write-crlf (player-stream player)
     (format nil "Armor: ~a (~+d armor)"
             (item-name (player-equipped-armor player))
             (item-armor (player-equipped-armor player)))))
  ;; Show vehicle condition if in a vehicle
  (when (player-vehicle player)
    (let* ((vehicle-item (player-vehicle player))
           (vehicle-template (mud.world::find-vehicle (mud.inventory:item-name vehicle-item))))
      (when vehicle-template
        (let ((current-armor (mud.world::vehicle-armor vehicle-template))
              (max-armor (mud.world::vehicle-max-armor vehicle-template)))
          (write-crlf (player-stream player)
           (format nil "Vehicle: ~a (~d/~d armor)"
                   (mud.inventory:item-name vehicle-item)
                   current-armor max-armor))
          (when (< current-armor max-armor)
            (write-crlf (player-stream player)
             (format nil "Condition: ~a"
                     (if (zerop current-armor) "BROKEN" "DAMAGED")))))))))

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
  (write-crlf (player-stream player) "  Social: say <text> (local), chat <text> (global), who")
  (write-crlf (player-stream player)
   "  Combat: attack <mob>, cast <spell> <target>, stats, spells")
  (write-crlf (player-stream player)
   "  Equipment: equip <item>, unequip weapon/armor")
  (write-crlf (player-stream player)
   "  Inventory: inventory (inv/i), use <item>, drop <item>, get <item> (loot corpses)")
  (write-crlf (player-stream player)
   "  Trade: shop [merchant], buy <item> [from <merchant>], sell <item> [to <merchant>]")
  (write-crlf (player-stream player)
   "  Quests: quest, quest start apple, status")
  (write-crlf (player-stream player)
   "  Other: help, quit, save, . (repeat last command), suicide (test death)")
  )

(define-command (("save") command-save) (player rest)
  (declare (ignore rest))
  (handler-case
      (let ((count (force-save-game player)))
        (cond
          ((null count)
           (write-crlf (player-stream player)
            (wrap "Save function not available." :bright-red)))
          ((zerop count)
           (write-crlf (player-stream player)
            (wrap "No players to save." :bright-yellow)))
          (t
           (write-crlf (player-stream player)
            (wrap (format nil "Game saved successfully (~d player~:p saved)." count) :bright-green)))))
    (error (err)
      (write-crlf (player-stream player)
       (wrap (format nil "Save failed: ~a" err) :bright-red)))))

(define-command (("quit") command-quit) (player rest)
  (declare (ignore rest))
  :quit)

(define-command ((".") command-repeat) (player rest)
  (declare (ignore rest))
  (write-crlf (player-stream player)
   (wrap "No previous command to repeat." :bright-red)))

(define-command (("say") command-say) (player rest)
  (handle-say player rest))

(define-command (("chat") command-chat) (player rest)
  (handle-chat player rest))

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
