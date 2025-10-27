(in-package :mud.server)

(defun force-save-game (player)
  "Force save all players' state to disk (same logic as periodic save)"
  (declare (ignore player)) ; We save all players, not just the current one
  (mud.server:save-game-state :reason :manual))

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
  (declare (ignore rest))
  (let ((active-quests (get-active-quests player))
        (completed-quests (get-completed-quests player)))
    ;; Show active quests
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
          "You have no active quests. Talk to NPCs to find quests!"
          :bright-yellow)))
    
    ;; Show completed quests
    (when completed-quests
      (write-crlf (player-stream player)
       (wrap "Completed Quests:" :bright-green))
      (dolist (quest completed-quests)
        (let ((repeatable-text (if (mud.quest::quest-repeatable quest) " (repeatable)" "")))
          (write-crlf (player-stream player)
           (format nil "  ~a~a: ~a"
                   (wrap (quest-name quest) :bright-magenta)
                   repeatable-text
                   (quest-description quest))))))))

(define-command (("quest-reset") command-quest-reset) (player rest)
  "Reset a completed quest so it can be repeated"
  (if (zerop (length rest))
      (write-crlf (player-stream player)
       (wrap "Reset which quest? Usage: quest-reset <quest-name>" :bright-red))
      (let* ((quest-name (string-trim '(#\  #\Tab) rest))
             (quest-id (cond
                        ((string-equal quest-name "apple") :apple-picking)
                        ((string-equal quest-name "pirate") :pirate-treasure)
                        ((string-equal quest-name "treasure") :pirate-treasure)
                        (t (intern (string-upcase quest-name) :keyword)))))
        (write-crlf (player-stream player)
         (wrap (mud.quest::reset-quest player quest-id) :bright-cyan)))))

(define-command (("auto-fight") command-auto-fight) (player rest)
  "Toggle auto-fight mode on/off"
  (declare (ignore rest))
  (setf (mud.player::player-auto-fight player) (not (mud.player::player-auto-fight player)))
  (write-crlf (player-stream player)
   (wrap (format nil "Auto-fight is now ~a" 
                 (if (mud.player::player-auto-fight player) "ON" "OFF"))
         :bright-cyan)))

(define-command (("help") command-help) (player rest)
  (declare (ignore rest))
  (write-crlf (player-stream player)
   (wrap "Commands:" :bright-yellow))
  (write-crlf (player-stream player)
   "  Movement: look (l), map, go <dir> (n/s/e/w/u/d/ne/nw/se/sw), use <vehicle>, uber <location>")
  (write-crlf (player-stream player) "  Social: say <text> (local), chat <text> (global), talk <npc>, who")
  (write-crlf (player-stream player)
   "  Combat: attack <mob>, cast <spell> <target>, stats, spells, auto-fight")
  (write-crlf (player-stream player)
   "  Equipment: equip <item>, unequip weapon/armor")
  (write-crlf (player-stream player)
   "  Inventory: inventory (inv/i), use <item> (including vehicles), drop <item>, get <item> (loot corpses)")
  (write-crlf (player-stream player)
   "  Vehicle: use <vehicle> (enter/exit), repair (fix broken vehicle with repair kit)")
  (write-crlf (player-stream player)
   "  Trade: shop [merchant], buy <item> [from <merchant>], sell <item> [to <merchant>], sell all [to <merchant>]")
  (write-crlf (player-stream player)
   "  Quests: quest, quest-reset <quest>, status")
  (write-crlf (player-stream player)
   "  Other: help, quit, save, time [HH:MM], . (repeat last command), locate <mob>, suicide (test death)")
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

(defun get-mob-dialogue (mob player)
  "Get dialogue for a mob based on its ID and player's quest state"
  (let ((mob-id (mud.mob::mob-id mob))
        (quest-giver (mud.mob::mob-quest-giver mob)))
    (cond
      ;; Quest giver dialogue
      (quest-giver
       (let ((quest-state (mud.quest::get-player-quest-data player quest-giver)))
         (cond
           ((eq quest-state :not-started)
            ;; Auto-start quest and give offer dialogue
            (mud.quest::start-quest player quest-giver)
            (get-quest-offer-dialogue mob-id quest-giver))
           ((eq quest-state :in-progress)
            ;; Check if quest is complete (player has required item)
            (multiple-value-bind (completed leveled-up quest)
                (mud.quest::check-quest-completion player quest-giver)
              (if completed
                  (progn
                    ;; Quest completed! Show completion dialogue and level up message
                    (let ((completion-dialogue (get-quest-completion-dialogue mob-id quest-giver)))
                      (if leveled-up
                          (format nil "~a~%~%You have gained a level!" completion-dialogue)
                          completion-dialogue)))
                  ;; Quest still in progress
                  (get-quest-progress-dialogue mob-id quest-giver))))
           ((eq quest-state :completed)
            (get-quest-completion-dialogue mob-id quest-giver)))))
      ;; Captain Blackbeard specific dialogue (fallback for non-quest-giver logic)
      ((eq mob-id :captain-blackbeard)
       (let ((quest-state (mud.quest::get-player-quest-data player :pirate-treasure)))
         (cond
           ((eq quest-state :not-started)
            ;; Auto-start quest
            (mud.quest::start-quest player :pirate-treasure)
            "Captain Blackbeard turns to you with a troubled look. 'Ahoy there, matey! I'm in a right pickle, I am. Me treasure map has gone missing, and without it, I'll never find me buried gold! If ye could help an old pirate out and find that map, I'd be mighty grateful. There's a reward in it for ye!'")
           ((eq quest-state :in-progress)
            ;; Check if quest is complete
            (multiple-value-bind (completed leveled-up quest)
                (mud.quest::check-quest-completion player :pirate-treasure)
              (if completed
                  (if leveled-up
                      "Captain Blackbeard's eyes light up! 'Ah, me treasure map! Ye've found it! Here's yer reward, matey - and keep that cutlass, it's served me well!'~%~%You have gained a level!"
                      "Captain Blackbeard's eyes light up! 'Ah, me treasure map! Ye've found it! Here's yer reward, matey - and keep that cutlass, it's served me well!'")
                  "Captain Blackbeard looks at you hopefully. 'Have ye found me treasure map yet, matey? I'm counting on ye!'")))
           ((eq quest-state :completed)
            "Captain Blackbeard grins broadly. 'Ah, the one who found me map! Ye've done me a great service, matey. May the winds be ever in yer favor!'"))))
      ;; Default dialogue for other mobs
      (t
       (format nil "~a doesn't seem interested in talking right now." (mud.mob::mob-name mob))))))

(defun get-quest-offer-dialogue (mob-id quest-id)
  "Get dialogue when offering a quest"
  (cond
    ((eq quest-id :apple-picking)
     (format nil "The Village Elder looks up at you with kind eyes. 'Ah, young adventurer! I could use your help. I need a fresh apple from the garden for my evening tea. It's a simple task, but I'm too old to make the journey myself. Would you be so kind as to fetch one for me?'"))
    ((eq quest-id :pirate-treasure)
     (format nil "Captain Blackbeard turns to you with a troubled look. 'Ahoy there, matey! I'm in a right pickle, I am. Me treasure map has gone missing, and without it, I'll never find me buried gold! If ye could help an old pirate out and find that map, I'd be mighty grateful. There's a reward in it for ye!'"))
    (t
     (format nil "~a has a quest for you!" (mud.mob::mob-name (mud.mob::find-mob-template mob-id))))))

(defun get-quest-progress-dialogue (mob-id quest-id)
  "Get dialogue when quest is in progress"
  (cond
    ((eq quest-id :apple-picking)
     "The Village Elder smiles warmly. 'Have you found that apple yet, dear? I'm looking forward to my evening tea.'")
    ((eq quest-id :pirate-treasure)
     "Captain Blackbeard looks at you hopefully. 'Have ye found me treasure map yet, matey? I'm counting on ye!'")
    (t
     (format nil "~a asks about your progress on their quest." (mud.mob::mob-name (mud.mob::find-mob-template mob-id))))))

(defun get-quest-completion-dialogue (mob-id quest-id)
  "Get dialogue when quest is completed (post-completion, not the reward message)"
  (cond
    ((eq quest-id :apple-picking)
     "The Village Elder smiles fondly at you. 'Thank you again for that apple, young one. It made for a wonderful evening tea.'")
    ((eq quest-id :pirate-treasure)
     "Captain Blackbeard gives you a friendly nod. 'Ahoy there, friend! Thanks again for finding me treasure map. Ye're always welcome aboard me ship!'")
    (t
     (format nil "~a greets you warmly." (mud.mob::mob-name (mud.mob::find-mob-template mob-id))))))

(define-command (("talk") command-talk) (player rest)
  (if (zerop (length rest))
      (write-crlf (player-stream player)
       (wrap "Talk to whom? Usage: talk <npc>" :bright-red))
      (let* ((target-name (string-trim '(#\  #\Tab) rest))
             (room-id (player-room player))
             (mob (mud.mob::find-mob-in-room room-id target-name)))
        (cond
          ;; Mob found - get dialogue based on mob ID
          (mob
           (write-crlf (player-stream player)
            (wrap (get-mob-dialogue mob player) :bright-cyan)))
          ;; No mob found
          (t
           (write-crlf (player-stream player)
            (wrap (format nil "You don't see anyone named ~a here to talk to." target-name) :bright-red)))))))


(defun find-quest-giver-in-room (room-id)
  "Find a quest giver in the current room"
  (let ((mobs (mud.mob::get-mobs-in-room room-id)))
    (find-if (lambda (mob) (mud.mob::mob-quest-giver mob)) mobs)))

(define-command ((".") command-repeat) (player rest)
  (declare (ignore rest))
  (write-crlf (player-stream player)
   (wrap "No previous command to repeat." :bright-red)))

(define-command (("say") command-say) (player rest)
  (handle-say player rest))

(define-command (("chat") command-chat) (player rest)
  (handle-chat player rest))

(define-command (("locate") command-locate) (player rest)
  "Locate a specific mob in the game world"
  (if (zerop (length rest))
      (write-crlf (player-stream player)
       (wrap "Locate which mob? Usage: locate <mob-name>" :bright-red))
      (let* ((mob-name (string-trim '(#\  #\Tab) rest))
             (found-mobs nil))
        ;; Search through all rooms for mobs matching the name
        (maphash (lambda (room-id mobs)
                   (dolist (mob mobs)
                     (when (search mob-name (mud.mob::mob-name mob) :test #'char-equal)
                       (push (list mob room-id) found-mobs))))
                 mud.mob::*room-mobs*)
        
        (if found-mobs
            (progn
              (write-crlf (player-stream player)
               (wrap (format nil "Found ~d mob(s) matching '~a':" (length found-mobs) mob-name) :bright-yellow))
              (dolist (mob-room found-mobs)
                (let ((mob (first mob-room))
                      (room-id (second mob-room)))
                  (let ((room (mud.world::find-room room-id)))
                    (write-crlf (player-stream player)
                     (format nil "  ~a is in ~a"
                             (wrap (mud.mob::mob-name mob) :bright-cyan)
                             (wrap (if room 
                                       (mud.world::room-name room)
                                       (format nil "Unknown Room (~a)" room-id))
                                   :bright-green)))))))
            (write-crlf (player-stream player)
             (wrap (format nil "No mobs found matching '~a'" mob-name) :bright-red))))))

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
