(in-package :mud.mob)

;;; Mob (Monster) System

(defstruct mob
  id              ; Unique identifier (keyword)
  name            ; Display name
  description     ; Description when examined
  health          ; Current health
  max-health      ; Maximum health
  damage          ; Base damage for attacks
  armor           ; Armor rating (reduces incoming damage)
  xp-reward       ; XP given when killed
  loot-table      ; List of item names that can drop
  aggressive      ; If T, mob attacks players on sight
  current-room    ; Current room ID where mob is located
  last-move-time  ; Universal time of last movement
  move-interval   ; Random interval between movements (in seconds)
  in-combat       ; If T, mob is in combat
  combat-target   ; Player or mob this mob is fighting
  last-attack-time ; Universal time of last attack
  attack-interval ; Interval between attacks (in seconds)
  )

(defparameter *mob-templates* (make-hash-table :test #'eq)
  "Hash table of mob-id -> mob template")

(defparameter *room-mobs* (make-hash-table :test #'eq)
  "Hash table of room-id -> list of mob instances in that room")

(defun define-mob-template (id name description max-health damage armor xp-reward loot-table &key (aggressive nil) (move-interval-min 30) (move-interval-max 120))
  "Define a mob template"
  (setf (gethash id *mob-templates*)
        (make-mob :id id
                  :name name
                  :description description
                  :health max-health
                  :max-health max-health
                  :damage damage
                  :armor armor
                  :xp-reward xp-reward
                  :loot-table loot-table
                  :aggressive aggressive
                  :current-room nil
                  :last-move-time 0
                  :move-interval (+ move-interval-min (random (- move-interval-max move-interval-min)))
                  :in-combat nil
                  :combat-target nil
                  :last-attack-time 0
                  :attack-interval 3)))

(defun find-mob-template (id)
  "Find a mob template by ID"
  (gethash id *mob-templates*))

(defun spawn-mob (template-id room-id)
  "Spawn a mob instance in a room from a template"
  (let ((template (find-mob-template template-id)))
    (when template
      ;; Create a fresh copy of the template
      (let ((mob-instance (copy-mob template)))
        ;; Reset health to max
        (setf (mob-health mob-instance) (mob-max-health mob-instance))
        ;; Set current room and initialize movement tracking
        (setf (mob-current-room mob-instance) room-id)
        (setf (mob-last-move-time mob-instance) (get-universal-time))
        ;; Generate new random move interval
        (setf (mob-move-interval mob-instance) 
              (+ (mob-move-interval template) (random 60)))
        ;; Initialize combat tracking
        (setf (mob-in-combat mob-instance) nil)
        (setf (mob-combat-target mob-instance) nil)
        (setf (mob-last-attack-time mob-instance) 0)
        (setf (mob-attack-interval mob-instance) 3)
        ;; Add to room
        (push mob-instance (gethash room-id *room-mobs*))
        mob-instance))))

(defun get-mobs-in-room (room-id)
  "Get list of mobs in a room"
  (gethash room-id *room-mobs*))

(defun find-mob-in-room (room-id mob-name)
  "Find a mob in a room by name (case-insensitive, partial match)"
  (let ((search-name (string-downcase mob-name))
        (mobs (get-mobs-in-room room-id)))
    (find-if (lambda (mob)
               (search search-name (string-downcase (mob-name mob))))
             mobs)))

(defun remove-mob-from-room (room-id mob)
  "Remove a mob from a room"
  (setf (gethash room-id *room-mobs*)
        (remove mob (gethash room-id *room-mobs*) :test #'eq)))

(defun mob-alive-p (mob)
  "Check if mob is alive"
  (> (mob-health mob) 0))

(defun damage-mob (mob amount)
  "Apply damage to a mob, return T if mob died"
  (setf (mob-health mob) (max 0 (- (mob-health mob) amount)))
  (not (mob-alive-p mob)))

(defun get-mob-loot (mob)
  "Generate loot from a mob's loot table"
  (when (mob-loot-table mob)
    ;; For now, just return all items in the loot table
    ;; Could add probability/randomness later
    (mapcar (lambda (item-name)
              (mud.inventory::create-item item-name))
            (mob-loot-table mob))))

(defun initialize-mobs ()
  "Initialize mob templates and clear room mobs"
  (clrhash *mob-templates*)
  (clrhash *room-mobs*)

  ;; Define basic mob templates

  ;; Goblin - weak enemy, drops basic loot
  (define-mob-template :goblin
                       "a goblin scout"
                       "A small, green-skinned creature with beady eyes and sharp teeth. It clutches a crude dagger."
                       30    ; max-health
                       8     ; damage
                       2     ; armor
                       50    ; xp-reward
                       '("rusty-dagger")  ; loot
                       :aggressive nil)

  ;; Wolf - medium enemy
  (define-mob-template :wolf
                       "a grey wolf"
                       "A lean grey wolf with fierce yellow eyes and bared fangs."
                       40    ; max-health
                       12    ; damage
                       3     ; armor
                       75    ; xp-reward
                       '("wolf-pelt")
                       :aggressive t)

  ;; Skeleton - undead enemy with better loot
  (define-mob-template :skeleton
                       "a skeleton warrior"
                       "An animated skeleton clad in rusted armor, wielding an ancient sword. Its hollow eye sockets glow with an eerie light."
                       50    ; max-health
                       15    ; damage
                       5     ; armor
                       100   ; xp-reward
                       '("bone-sword" "rusted-chainmail")
                       :aggressive t)

  ;; Bandit - human enemy with decent loot
  (define-mob-template :bandit
                       "a bandit"
                       "A rough-looking brigand wearing leather armor and carrying a well-maintained sword."
                       60    ; max-health
                       18    ; damage
                       8     ; armor
                       125   ; xp-reward
                       '("steel-sword" "leather-armor" "gold-coins")
                       :aggressive nil)

  ;; Boss: Forest Guardian - tough enemy
  (define-mob-template :forest-guardian
                       "the Forest Guardian"
                       "A massive bear-like creature covered in moss and vines. Ancient magic radiates from its form, and its eyes hold an otherworldly intelligence."
                       150   ; max-health
                       25    ; damage
                       12    ; armor
                       300   ; xp-reward
                       '("guardian-axe" "nature-amulet")
                       :aggressive t)

  ;; Spawn initial mobs in the world
  (spawn-mob :goblin 'mud.world::whispering-wood)
  (spawn-mob :wolf 'mud.world::whispering-wood)
  (spawn-mob :skeleton 'mud.world::graveyard)
  (spawn-mob :bandit 'mud.world::moonlit-lane)
  (spawn-mob :forest-guardian 'mud.world::ancient-grove))

;;; Mob Movement System

(defun get-adjacent-rooms (room-id)
  "Get list of room IDs adjacent to the given room"
  (let ((room (mud.world::find-room room-id)))
    (when room
      (mapcar #'cdr (mud.world::room-exits room)))))

(defun move-mob-to-room (mob new-room-id)
  "Move a mob from its current room to a new room"
  (let ((old-room-id (mob-current-room mob)))
    (when (and old-room-id new-room-id)
      ;; Remove from old room
      (setf (gethash old-room-id *room-mobs*)
            (remove mob (gethash old-room-id *room-mobs*) :test #'eq))
      ;; Add to new room
      (push mob (gethash new-room-id *room-mobs*))
      ;; Update mob's current room
      (setf (mob-current-room mob) new-room-id)
      ;; Update last move time
      (setf (mob-last-move-time mob) (get-universal-time))
      ;; Generate new random move interval
      (setf (mob-move-interval mob) 
            (+ 30 (random 90))) ; 30-120 seconds
      t)))

(defun should-mob-move-p (mob)
  "Check if a mob should move based on its movement interval"
  (let ((current-time (get-universal-time))
        (last-move (mob-last-move-time mob))
        (interval (mob-move-interval mob)))
    (>= (- current-time last-move) interval)))

(defun get-random-adjacent-room (room-id)
  "Get a random adjacent room ID, or nil if none available"
  (let ((adjacent-rooms (get-adjacent-rooms room-id)))
    (when adjacent-rooms
      (nth (random (length adjacent-rooms)) adjacent-rooms))))

(defun process-mob-movement (mob)
  "Process movement for a single mob if it's time to move"
  (when (and (mob-alive-p mob)
             (should-mob-move-p mob)
             (mob-current-room mob))
    (let ((old-room-id (mob-current-room mob))
          (new-room-id (get-random-adjacent-room (mob-current-room mob))))
      (when new-room-id
        (move-mob-to-room mob new-room-id)
        ;; Return the mob and rooms for announcement
        (values mob old-room-id new-room-id)))))

(defun process-all-mob-movements ()
  "Process movement for all mobs in the game"
  (let ((movements nil))
    (maphash (lambda (room-id mobs)
               (declare (ignore room-id))
               (when mobs ; Check if mobs is not nil
                 (dolist (mob mobs)
                   (multiple-value-bind (moved-mob old-room new-room)
                       (process-mob-movement mob)
                     (when moved-mob
                       (push (list moved-mob old-room new-room) movements))))))
             *room-mobs*)
    movements))

;;; Aggressive Mob System

(defun mob-aggressive-p (mob)
  "Check if a mob is aggressive"
  (mob-aggressive mob))

(defun get-aggressive-mobs-in-room (room-id)
  "Get list of aggressive mobs in a room"
  (remove-if-not #'mob-aggressive-p (get-mobs-in-room room-id)))

(defun should-mob-attack-player (mob player)
  "Check if an aggressive mob should attack a player"
  (and (mob-aggressive-p mob)
       (mob-alive-p mob)
       (mud.player::player-alive-p player)
       (eq (mob-current-room mob) (mud.player::player-room player))))

;;; Automatic Combat System

(defun start-combat (mob target)
  "Start combat between mob and target"
  (setf (mob-in-combat mob) t)
  (setf (mob-combat-target mob) target)
  (setf (mob-last-attack-time mob) (get-universal-time)))

(defun end-combat (mob)
  "End combat for a mob"
  (setf (mob-in-combat mob) nil)
  (setf (mob-combat-target mob) nil))

(defun mob-in-combat-p (mob)
  "Check if a mob is in combat"
  (mob-in-combat mob))

(defun should-mob-attack-in-combat (mob)
  "Check if a mob should attack during combat"
  (and (mob-in-combat-p mob)
       (mob-alive-p mob)
       (mob-combat-target mob)
       (let ((target (mob-combat-target mob)))
         (and target
              (if (mud.player::player-p target)
                  (mud.player::player-alive-p target)
                  (mob-alive-p target))
              (eq (mob-current-room mob) 
                  (if (mud.player::player-p target)
                      (mud.player::player-room target)
                      (mob-current-room target)))
              (>= (- (get-universal-time) (mob-last-attack-time mob))
                  (mob-attack-interval mob))))))

(defun process-mob-combat-attack (mob)
  "Process a combat attack for a mob"
  (when (should-mob-attack-in-combat mob)
    (let ((target (mob-combat-target mob)))
      (setf (mob-last-attack-time mob) (get-universal-time))
      (if (mud.player::player-p target)
          ;; Attack player
          (mud.server::handle-mob-attack-player mob target)
          ;; Attack other mob (not implemented yet)
          nil))))

(defun process-all-mob-combat ()
  "Process combat for all mobs in the game"
  (let ((combat-actions nil))
    (maphash (lambda (room-id mobs)
               (declare (ignore room-id))
               (when mobs ; Check if mobs is not nil
                 (dolist (mob mobs)
                   (when (mob-in-combat-p mob)
                     (let ((action (process-mob-combat-attack mob)))
                       (when action
                         (push action combat-actions)))))))
             *room-mobs*)
    combat-actions))
