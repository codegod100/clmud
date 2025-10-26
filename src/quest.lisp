(in-package :mud.quest)

;;; Quest System

(defstruct quest
  id
  name
  description
  completion-check  ; Function that takes a player and returns T if quest is complete
  reward-xp
  reward-text
  reward-items      ; List of item names to give as rewards
  repeatable)       ; If T, quest can be reset and repeated

(defparameter *quests* (make-hash-table :test #'eq)
  "Hash table of quest-id -> quest struct")

(defun define-quest (id name description completion-check reward-xp reward-text &key (repeatable nil) (reward-items nil))
  "Define a quest"
  (setf (gethash id *quests*)
        (make-quest :id id
                    :name name
                    :description description
                    :completion-check completion-check
                    :reward-xp reward-xp
                    :reward-text reward-text
                    :reward-items reward-items
                    :repeatable repeatable)))

(defun find-quest (quest-id)
  "Find a quest by ID"
  (gethash quest-id *quests*))

(defun get-player-quest-data (player quest-id)
  "Get quest data for a player (returns :not-started, :in-progress, or :completed)"
  (let ((quest-state (mud.player::player-quest-state player)))
    (if quest-state
        (gethash quest-id quest-state :not-started)
        :not-started)))

(defun set-player-quest-data (player quest-id state)
  "Set quest state for a player"
  (unless (mud.player::player-quest-state player)
    (setf (mud.player::player-quest-state player) (make-hash-table :test #'eq)))
  (setf (gethash quest-id (mud.player::player-quest-state player)) state))

(defun start-quest (player quest-id)
  "Start a quest for a player"
  (let ((quest (find-quest quest-id)))
    (if quest
        (let ((current-state (get-player-quest-data player quest-id)))
          (cond
            ((and (eq current-state :completed) (not (quest-repeatable quest)))
             "You have already completed this quest!")
            ((eq current-state :in-progress)
             "You are already on this quest!")
            (t
             (set-player-quest-data player quest-id :in-progress)
             (format nil "Quest started: ~a~%~a"
                     (quest-name quest)
                     (quest-description quest)))))
        "Quest not found!")))

(defun reset-quest (player quest-id)
  "Reset a quest for a player (only if quest is repeatable)"
  (let ((quest (find-quest quest-id)))
    (if quest
        (if (quest-repeatable quest)
            (progn
              (set-player-quest-data player quest-id :not-started)
              (format nil "Quest reset: ~a is now available again." (quest-name quest)))
            "This quest cannot be reset - it's not repeatable!")
        "Quest not found!")))

(defun check-quest-completion (player quest-id)
  "Check if a quest is complete and award XP and items if so"
  (let ((quest (find-quest quest-id))
        (state (get-player-quest-data player quest-id)))
    (when (and quest (eq state :in-progress))
      (when (funcall (quest-completion-check quest) player)
        (set-player-quest-data player quest-id :completed)
        ;; Award XP
        (let ((leveled-up (mud.player::award-xp player (quest-reward-xp quest))))
          ;; Award items
          (when (quest-reward-items quest)
            (dolist (item-name (quest-reward-items quest))
              (let ((item (mud.inventory::create-item item-name)))
                (when item
                  (mud.inventory::add-to-inventory player item)))))
          (values t leveled-up quest))))))

(defun get-active-quests (player)
  "Get list of active quests for a player"
  (let ((active-quests nil))
    (when (mud.player::player-quest-state player)
      (maphash (lambda (quest-id state)
                 (when (eq state :in-progress)
                   (let ((quest (find-quest quest-id)))
                     (when quest
                       (push quest active-quests)))))
               (mud.player::player-quest-state player)))
    active-quests))

(defun get-completed-quests (player)
  "Get list of completed quests for a player"
  (let ((completed-quests nil))
    (when (mud.player::player-quest-state player)
      (maphash (lambda (quest-id state)
                 (when (eq state :completed)
                   (let ((quest (find-quest quest-id)))
                     (when quest
                       (push quest completed-quests)))))
               (mud.player::player-quest-state player)))
    completed-quests))

(defun has-item-in-inventory-p (player item-name)
  "Check if player has an item with the given name in inventory"
  (let ((inv (mud.player::player-inventory player)))
    (some (lambda (item)
            (string-equal (mud.inventory::item-name item) item-name))
          inv)))

(defun initialize-quests ()
  "Initialize all quests"
  (clrhash *quests*)

  ;; The Apple Quest - simple starter quest (repeatable)
  (define-quest :apple-picking
                "The First Harvest"
                "The village elder needs a fresh apple from the garden. Pick up an apple and return it to earn your first reward."
                (lambda (player)
                  ;; Quest is complete if player has an apple in inventory
                  (has-item-in-inventory-p player "apple"))
                200  ; Exactly enough XP to level from 1 to 2
                "The elder smiles warmly. 'Thank you, young adventurer. Your journey has just begun!'"
                :repeatable t)

  ;; The Pirate's Treasure Quest - adventure in the cove (repeatable)
  (define-quest :pirate-treasure
                "The Pirate's Lost Treasure"
                "Captain Blackbeard in the hidden cove has lost his treasure map! He's offering a reward to anyone who can help him find it. Talk to him to learn more about his lost treasure."
                (lambda (player)
                  ;; Quest is complete if player has the treasure map
                  (has-item-in-inventory-p player "treasure-map"))
                300  ; Good XP reward for a more complex quest
                "Captain Blackbeard's eyes light up! 'Ah, me treasure map! Ye've found it! Here's yer reward, matey - and keep that cutlass, it's served me well!'"
                :repeatable t
                :reward-items '("pirate-cutlass")))

(defun maybe-announce-quest-rewards (player)
  "Check all active quests for completion and announce rewards"
  (let ((completed-quests nil))
    (when (mud.player::player-quest-state player)
      (maphash (lambda (quest-id state)
                 (when (eq state :in-progress)
                   (multiple-value-bind (completed leveled-up quest)
                       (check-quest-completion player quest-id)
                     (when completed
                       (push (list quest leveled-up) completed-quests)))))
               (mud.player::player-quest-state player)))
    (when completed-quests
      (let ((stream (mud.player::player-stream player)))
        (dolist (quest-data completed-quests)
          (let ((quest (first quest-data))
                (leveled-up (second quest-data)))
            (mud.server::write-crlf stream
             (mud.ansi::wrap (quest-reward-text quest) :bright-green))
            (when leveled-up
              (mud.server::write-crlf stream
               (mud.ansi::wrap "You have gained a level!" :bright-yellow)))))))))
