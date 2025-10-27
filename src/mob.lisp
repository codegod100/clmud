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
  inventory       ; List of items the mob is carrying/equipped
  aggressive      ; If T, mob attacks players on sight
  current-room    ; Current room ID where mob is located
  last-move-time  ; Universal time of last movement
  move-interval   ; Random interval between movements (in seconds)
  in-combat       ; If T, mob is in combat
  combat-target   ; Player or mob this mob is fighting
  last-attack-time ; Universal time of last attack
  attack-interval ; Interval between attacks (in seconds)
  quest-giver     ; Quest ID this mob can give (nil if not a quest giver)
  vehicle         ; Vehicle the mob is using (nil if none)
  faction         ; Faction ID this mob belongs to (nil if neutral)
  )

(defparameter *mob-templates* (make-hash-table :test #'eq)
  "Hash table of mob-id -> mob template")

(defparameter *room-mobs* (make-hash-table :test #'eq)
  "Hash table of room-id -> list of mob instances in that room")

;; Global autofight state - tracks which players should autofight on next tick
(defparameter *autofight-players* (make-hash-table :test #'eq)
  "Hash table of player -> mob they should autofight against")

(defun define-mob-template (id name description max-health damage armor xp-reward loot-table &key (aggressive nil) (move-interval-min 30) (move-interval-max 120) (quest-giver nil) (vehicle nil) (faction nil))
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
                  :inventory nil
                  :aggressive aggressive
                  :current-room nil
                  :last-move-time 0
                  :move-interval (+ move-interval-min (random (- move-interval-max move-interval-min)))
                  :in-combat nil
                  :combat-target nil
                  :last-attack-time 0
                  :attack-interval 3
                  :quest-giver quest-giver
                  :vehicle vehicle
                  :faction faction)))

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
        ;; Initialize inventory
        (setf (mob-inventory mob-instance) nil)
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
  (let ((remaining-mobs (remove mob (gethash room-id *room-mobs*) :test #'eq)))
    (setf (gethash room-id *room-mobs*)
          (if (listp remaining-mobs) remaining-mobs nil))))

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
    (remove nil (mapcar (lambda (item-name)
                          (mud.inventory::create-item item-name))
                        (mob-loot-table mob)))))

(defun add-item-to-mob-inventory (mob item)
  "Add an item to a mob's inventory"
  (when (and mob item)
    (push item (mob-inventory mob))))

(defun remove-item-from-mob-inventory (mob item)
  "Remove an item from a mob's inventory"
  (when (and mob item)
    (setf (mob-inventory mob)
          (remove item (mob-inventory mob) :test #'eq))))

(defun find-item-in-mob-inventory (mob item-name)
  "Find an item in a mob's inventory by name"
  (when mob
    (find-if (lambda (item)
               (string-equal (mud.inventory::item-name item) item-name))
             (mob-inventory mob))))

(defun equip-item-to-mob (mob item)
  "Equip an item to a mob (add to inventory)"
  (add-item-to-mob-inventory mob item))

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
                       "A rough-looking brigand wearing leather armor and carrying a well-maintained sword. A sleek black motorcycle sits nearby."
                       60    ; max-health
                       18    ; damage
                       8     ; armor
                       125   ; xp-reward
                       '("steel-sword" "leather-armor" "gold-coins")
                       :aggressive nil
                       :vehicle "motorcycle")

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

  ;; Village Elder - Quest giver for apple quest
  (define-mob-template :village-elder
                       "Village Elder"
                       "A wise old woman with kind eyes and weathered hands. She sits peacefully in the village square, watching over the community with gentle authority."
                       50    ; max-health
                       5     ; damage
                       2     ; armor
                       50    ; xp-reward
                       '("healing-potion" "wisdom-scroll")
                       :aggressive nil
                       :quest-giver :apple-picking)

  ;; Captain Blackbeard - Pirate boss in the cove
  (define-mob-template :captain-blackbeard
                       "Captain Blackbeard"
                       "A grizzled pirate captain with a long black beard and a wicked gleam in his eye. His cutlass gleams in the moonlight, and a treasure map peeks from his coat pocket."
                       80    ; max-health
                       20    ; damage
                       10    ; armor
                       200   ; xp-reward
                       '("pirate-cutlass" "treasure-map" "gold-coins" "pirate-hat")
                       :aggressive nil
                       :quest-giver :pirate-treasure)

  ;; Faction Mobs - Each faction has representative mobs

  ;; Royal Guard faction mobs
  (define-mob-template :royal-guard
                       "a Royal Guard soldier"
                       "A disciplined soldier in polished armor, carrying a gleaming sword. The Royal Guard emblem is proudly displayed on their breastplate."
                       60    ; max-health
                       18    ; damage
                       8     ; armor
                       120   ; xp-reward
                       '("royal-sword" "royal-armor" "guard-badge")
                       :aggressive nil
                       :faction :royal-guard)

  (define-mob-template :royal-commander
                       "Royal Guard Commander"
                       "A distinguished officer with years of military experience. Their armor bears the marks of many battles, and their sword is a masterwork of craftsmanship."
                       100   ; max-health
                       25    ; damage
                       12    ; armor
                       200   ; xp-reward
                       '("commander-sword" "commander-armor" "royal-seal")
                       :aggressive nil
                       :faction :royal-guard
                       :quest-giver :royal-guard-patrol)

  ;; Nomad Tribes faction mobs
  (define-mob-template :nomad-scout
                       "a nomad scout"
                       "A lean figure in leather armor, moving with the grace of one who has spent their life on the open plains. Their eyes are sharp and watchful."
                       45    ; max-health
                       15    ; damage
                       5     ; armor
                       90    ; xp-reward
                       '("nomad-bow" "leather-armor" "spirit-totem")
                       :aggressive nil
                       :faction :nomad-tribes)

  (define-mob-template :nomad-shaman
                       "a nomad shaman"
                       "A wise elder with intricate tattoos covering their weathered skin. They carry a staff topped with feathers and bones, and their eyes hold ancient knowledge."
                       70    ; max-health
                       20    ; damage
                       6     ; armor
                       150   ; xp-reward
                       '("shaman-staff" "spirit-robe" "ancient-totem")
                       :aggressive nil
                       :faction :nomad-tribes
                       :quest-giver :spirit-gathering)

  ;; Mountain Clans faction mobs
  (define-mob-template :mountain-miner
                       "a mountain miner"
                       "A sturdy dwarf with calloused hands and a pickaxe slung over their shoulder. Their beard is braided with metal rings, and their eyes sparkle with the joy of finding precious gems."
                       55    ; max-health
                       16    ; damage
                       7     ; armor
                       100   ; xp-reward
                       '("mining-pick" "miner-helmet" "raw-gems")
                       :aggressive nil
                       :faction :mountain-clans)

  (define-mob-template :clan-smith
                       "a clan smith"
                       "A master craftsman with massive arms and a leather apron. Their hammer rings against the anvil as they forge weapons and armor of exceptional quality."
                       80    ; max-health
                       22    ; damage
                       10    ; armor
                       180   ; xp-reward
                       '("smith-hammer" "forged-armor" "masterwork-sword")
                       :aggressive nil
                       :faction :mountain-clans
                       :quest-giver :ore-collection)

  ;; Shadow Cult faction mobs
  (define-mob-template :cult-acolyte
                       "a cult acolyte"
                       "A hooded figure in dark robes, their face hidden in shadow. They carry a tome bound in strange leather and whisper incantations under their breath."
                       50    ; max-health
                       14    ; damage
                       4     ; armor
                       80    ; xp-reward
                       '("dark-tome" "cult-robe" "shadow-dust")
                       :aggressive nil
                       :faction :shadow-cult)

  (define-mob-template :cult-master
                       "a cult master"
                       "A powerful figure in ornate dark robes, their eyes glowing with forbidden knowledge. Ancient symbols float around them, and the air crackles with dark energy."
                       90    ; max-health
                       28    ; damage
                       8     ; armor
                       220   ; xp-reward
                       '("master-tome" "dark-robe" "shadow-orb")
                       :aggressive nil
                       :faction :shadow-cult
                       :quest-giver :forbidden-knowledge)

  ;; Nature Guardians faction mobs
  (define-mob-template :forest-druid
                       "a forest druid"
                       "A peaceful figure in robes of living leaves, their skin marked with natural tattoos. They carry a staff of living wood and move with the grace of the forest itself."
                       65    ; max-health
                       19    ; damage
                       6     ; armor
                       130   ; xp-reward
                       '("druid-staff" "leaf-armor" "nature-seed")
                       :aggressive nil
                       :faction :nature-guardians)

  (define-mob-template :grove-keeper
                       "the Grove Keeper"
                       "An ancient guardian with bark-like skin and eyes that hold the wisdom of centuries. They are the protector of the sacred grove, wielding the power of nature itself."
                       120   ; max-health
                       30    ; damage
                       15    ; armor
                       300   ; xp-reward
                       '("ancient-staff" "bark-armor" "life-essence")
                       :aggressive nil
                       :faction :nature-guardians
                       :quest-giver :nature-balance)

  ;; Spawn initial mobs in the world
  (spawn-mob :goblin 'mud.world::whispering-wood)
  (spawn-mob :wolf 'mud.world::whispering-wood)
  (spawn-mob :skeleton 'mud.world::graveyard)
  (spawn-mob :bandit 'mud.world::moonlit-lane)
  (spawn-mob :forest-guardian 'mud.world::ancient-grove)
  (spawn-mob :village-elder 'mud.world::village-square)
  (spawn-mob :captain-blackbeard 'mud.world::hidden-cove)

  ;; Spawn faction mobs in their respective regions
  ;; Royal Guard faction
  (spawn-mob :royal-guard 'mud.world::northern-outpost)
  (spawn-mob :royal-commander 'mud.world::northern-outpost)
  
  ;; Nomad Tribes faction
  (spawn-mob :nomad-scout 'mud.world::eastern-plains)
  (spawn-mob :nomad-shaman 'mud.world::eastern-plains)
  
  ;; Mountain Clans faction
  (spawn-mob :mountain-miner 'mud.world::western-hills)
  (spawn-mob :clan-smith 'mud.world::western-hills)
  
  ;; Shadow Cult faction
  (spawn-mob :cult-acolyte 'mud.world::southern-desert)
  (spawn-mob :cult-master 'mud.world::southern-desert)
  
  ;; Nature Guardians faction
  (spawn-mob :forest-druid 'mud.world::spirit-grove)
  (spawn-mob :grove-keeper 'mud.world::spirit-grove))

;;; Mob Movement System

(defun get-adjacent-rooms (room-id &optional mob)
  "Get list of room IDs adjacent to the given room that the mob can access"
  (let ((room (mud.world::find-room room-id))
        (vehicle-type (when mob (mob-vehicle mob))))
    (when room
      (remove nil
              (mapcar (lambda (exit)
                        (let ((rest-of-entry (cdr exit)))
                          ;; Handle typed exits: (:direction :type . room-id)
                          (if (and (consp rest-of-entry) (keywordp (car rest-of-entry)))
                              ;; Typed exit - check if mob's vehicle can access it
                              (let ((exit-type (car rest-of-entry)))
                                (cond
                                  ;; Pedestrian exits - accessible to all mobs
                                  ((eq exit-type :pedestrian)
                                   (cdr rest-of-entry))
                                  ;; Ground exits - accessible to all mobs
                                  ((eq exit-type :ground)
                                   (cdr rest-of-entry))
                                  ;; Other vehicle types - only if mob has matching vehicle
                                  ((and vehicle-type (string-equal vehicle-type (string-downcase (symbol-name exit-type))))
                                   (cdr rest-of-entry))))
                              ;; Simple exit - always accessible to mobs
                              rest-of-entry)))
                      (mud.world::room-exits room))))))

(defun move-mob-to-room (mob new-room-id)
  "Move a mob from its current room to a new room"
  (let ((old-room-id (mob-current-room mob)))
    (when (and old-room-id new-room-id)
      ;; Remove from old room
      (let ((remaining-mobs (remove mob (gethash old-room-id *room-mobs*) :test #'eq)))
        (setf (gethash old-room-id *room-mobs*)
              (if (listp remaining-mobs) remaining-mobs nil)))
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

(defun get-random-adjacent-room (room-id &optional mob)
  "Get a random adjacent room ID, or nil if none available"
  (let ((adjacent-rooms (get-adjacent-rooms room-id mob)))
    (when adjacent-rooms
      (nth (random (length adjacent-rooms)) adjacent-rooms))))

(defun process-mob-movement (mob)
  "Process movement for a single mob if it's time to move"
  (when (and (mob-alive-p mob)
             (should-mob-move-p mob)
             (mob-current-room mob))
    (let ((old-room-id (mob-current-room mob))
          (new-room-id (get-random-adjacent-room (mob-current-room mob) mob)))
      (when new-room-id
        (move-mob-to-room mob new-room-id)
        ;; Return the mob and rooms for announcement
        (values mob old-room-id new-room-id)))))

(defun process-all-mob-movements ()
  "Process movement for all mobs in the game"
  (let ((movements nil))
    (maphash (lambda (room-id mobs)
               (declare (ignore room-id))
               (when (and mobs (listp mobs)) ; Check if mobs is not nil and is a list
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
  ;; If target is a player with autofight enabled, add to autofight queue
  (when (and (mud.player::player-p target)
             (mud.player::player-auto-fight target))
    (add-autofight-player target mob))
  (setf (mob-last-attack-time mob) (get-universal-time)))

(defun end-combat (mob)
  "End combat for a mob"
  (let ((target (mob-combat-target mob)))
    ;; Remove player from autofight queue if they were in it
    (when (mud.player::player-p target)
      (remove-autofight-player target))
    (setf (mob-in-combat mob) nil)
    (setf (mob-combat-target mob) nil)))

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

(defun process-autofight-actions ()
  "Process all autofight actions for players in combat"
  (maphash (lambda (player mob)
             (when (and (mud.player::player-alive-p player)
                        (mud.mob::mob-alive-p mob)
                        (mud.player::player-auto-fight player)
                        (eq (mud.player::player-room player) (mud.mob::mob-current-room mob)))
               ;; Player should autofight - trigger counter-attack
               (mud.server::auto-fight-counter-attack player mob)))
           *autofight-players*))

(defun add-autofight-player (player mob)
  "Add a player to the autofight queue"
  (setf (gethash player *autofight-players*) mob))

(defun remove-autofight-player (player)
  "Remove a player from the autofight queue"
  (remhash player *autofight-players*))

(defun process-all-mob-combat ()
  "Process combat for all mobs in the game"
  (let ((combat-actions nil))
    (maphash (lambda (room-id mobs)
               (declare (ignore room-id))
               (when (and mobs (listp mobs)) ; Check if mobs is not nil and is a list
                 (dolist (mob mobs)
                   (when (mob-in-combat-p mob)
                     ;; Process regular combat attacks
                     (let ((action (process-mob-combat-attack mob)))
                       (when action
                         (push action combat-actions)))))))
             *room-mobs*)
    ;; Process autofight actions for all players
    (process-autofight-actions)
    combat-actions))
