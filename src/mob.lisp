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
  )

(defparameter *mob-templates* (make-hash-table :test #'eq)
  "Hash table of mob-id -> mob template")

(defparameter *room-mobs* (make-hash-table :test #'eq)
  "Hash table of room-id -> list of mob instances in that room")

(defun define-mob-template (id name description max-health damage armor xp-reward loot-table &key (aggressive nil))
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
                  :aggressive aggressive)))

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
  (spawn-mob :goblin 'whispering-wood)
  (spawn-mob :wolf 'whispering-wood)
  (spawn-mob :skeleton 'graveyard)
  (spawn-mob :bandit 'moonlit-lane))
