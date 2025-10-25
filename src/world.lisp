(in-package :mud.world)

(defstruct room
  id
  name
  description
  exits
  (items nil :type list)
  (facets nil :type list))  ; List of (name . description) pairs for examinable details

(defparameter *rooms* (make-hash-table :test #'eq))
(defparameter *starting-room* 'village-square)

;;; Vehicle definitions
(defstruct vehicle
  name
  type
  description
  (damage 0 :type integer)     ; Damage when ramming
  (speed 0 :type integer))     ; Speed/momentum bonus

(defparameter *vehicles* (make-hash-table :test 'equal)
  "Hash table of vehicle name -> vehicle struct")

(defun define-vehicle (name type description &key (damage 0) (speed 0))
  "Define a vehicle that can be entered"
  (setf (gethash name *vehicles*)
        (make-vehicle :name name :type type :description description
                      :damage damage :speed speed)))

(defun find-vehicle (name)
  "Find a vehicle by name (case-insensitive)"
  (gethash (string-downcase name) *vehicles*))

(defun define-room (id name description exits &optional facets)
  (setf (gethash id *rooms*)
        (make-room :id id :name name :description description :exits exits :facets facets)))

(defun initialize-world ()
  (clrhash *rooms*)
  (clrhash *vehicles*)

  ;; Define vehicles
  (define-vehicle "skiff" :water "A small wooden boat that glides across water on its own.")
  (define-vehicle "boat" :water "A small wooden boat that glides across water on its own." :damage 5 :speed 2)
  (define-vehicle "eternal wanderer" :water "A small wooden boat that glides across water on its own." :damage 5 :speed 2)
  (define-vehicle "car" :uber "A sleek, magical carriage that can take you anywhere instantly." :damage 30 :speed 10)
  (define-vehicle "carriage" :uber "A sleek, magical carriage that can take you anywhere instantly." :damage 30 :speed 10)

  (define-room 'village-square
               "Village Square"
               "Cobblestone paths converge beneath an [ancient oak], its leaves whispering tales of heroes past. [Lanterns] sway gently, casting golden halos in the dusk."
               '((:north . tavern-common-room)
                 (:east . moonlit-lane)
                 (:south . village-garden)
                 (:west . market-stalls)
                 (:northwest . graveyard))
               '(("ancient oak" . "The massive oak has stood here for centuries. Carved into its trunk are names of heroes long past, and a small hollow near the base seems to hide something...")
                 ("lanterns" . "The lanterns are wrought iron, their flames never seeming to dim. They were said to be blessed by an ancient mage to guide lost travelers.")))
  (define-room 'tavern-common-room
               "The Bronze Badger"
               "Warm lamplight spills over polished oak tables, while the scent of spiced cider mingles with distant lute music. A crackling [hearth] invites weary travelers. Behind the bar, a [weathered map] hangs on the wall."
               '((:south . village-square)
                 (:up . tavern-loft))
               '(("hearth" . "The stone hearth crackles with eternal flame. Local legend says it was lit by the first settlers and has never gone out.")
                 ("weathered map" . "The map shows the surrounding lands, with several locations marked with red X's. One appears to be deep in the Whispering Wood...")))
  (define-room 'tavern-loft
               "Tavern Loft"
               "Low beams and soft straw mattresses offer respite. A narrow [window] reveals the silver glow of the moonlit treeline beyond the village walls."
               '((:down . tavern-common-room))
               '(("window" . "Through the dusty glass, you can see the distant forest. Something occasionally glimmers between the trees...")))
  (define-room 'moonlit-lane
               "Moonlit Lane"
               "A narrow lane stretches eastward, flanked by [ivy-clad cottages]. Fireflies dance in the night air, drawing the eye toward the shadowed [forest archway]."
               '((:west . village-square)
                 (:east . whispering-wood))
               '(("ivy-clad cottages" . "The cottages are ancient, their windows dark. The ivy seems to move slightly, as if breathing...")
                 ("forest archway" . "Two twisted trees form a natural archway leading into the Whispering Wood. Strange runes are carved into their bark.")))
  (define-room 'whispering-wood
               "Whispering Wood"
               "Towering pines murmur secrets overhead, their needles shimmering with dew. Somewhere deeper within, an [owl] hoots, beckoning the brave. A [standing stone] covered in moss rises from the forest floor."
               '((:west . moonlit-lane))
               '(("owl" . "The owl watches you with knowing eyes. It seems to be guarding something in the deeper woods...")
                 ("standing stone" . "Ancient runes cover the stone. You can barely make out the words: 'Only the worthy may pass beyond...'")))
  (define-room 'market-stalls
               "Closing Market"
               "Canopies ripple in the breeze as merchants shutter their stalls. The lingering aroma of [roasted chestnuts] and fresh parchment fills the air. A [notice board] displays various announcements."
               '((:east . village-square)
                 (:south . riverbank))
               '(("roasted chestnuts" . "The warm chestnuts are a local specialty. The vendor mentions that the recipe is a closely guarded secret passed down for generations.")
                 ("notice board" . "Several notices are pinned here: 'WANTED: Brave souls to investigate disappearances in Whispering Wood. Reward offered.' Another reads: 'Lost: Family heirloom ring, last seen near the riverbank.'")))
  (define-room 'riverbank
               "Riverbank"
               "Moonlight paints the [river] in silver ribbons. A wooden skiff knocks gently against the pier, ready for anyone bold enough to cast off."
               '((:north . market-stalls)
                 (:downstream :water . hidden-cove))
               '(("river" . "The water flows swiftly, its depths dark and mysterious. You think you see something glinting on the riverbed...")))
  (define-room 'hidden-cove
               "Hidden Cove"
               "A secluded cove surrounded by towering cliffs. The water is calm here, crystal clear. Ancient [ruins] peek through the vines on the cliff face, and a narrow [cave entrance] yawns in the rock."
               '((:upstream :water . riverbank))
               '(("ruins" . "The ruins are covered in the same strange runes you saw on the forest archway. They seem to glow faintly in the moonlight, pulsing with an otherworldly energy.")
                 ("cave entrance" . "The cave is dark and foreboding. Cold air wafts out, carrying whispers of things best left undisturbed. You sense great power - and great danger - within.")))
  (define-room 'graveyard
               "Graveyard"
               "Ancient [tombstones] lean in the mist, their inscriptions worn by time. The air is still, heavy with the weight of countless souls who have passed through this veil. A faint [ethereal glow] marks the boundary between life and death."
               '((:southeast . village-square))
               '(("tombstones" . "Most inscriptions are illegible, but one reads: 'Here lies the Keeper of Secrets. Death is not the end, merely a door.' Fresh flowers rest at its base, though no one living has been seen placing them.")
                 ("ethereal glow" . "The glow pulses gently, like a heartbeat. Those who have died and returned speak of a presence here - neither malevolent nor kind, simply... waiting.")))

  (define-room 'village-garden
               "Village Garden"
               "A small, peaceful garden bursting with life. Rows of vegetables grow alongside fragrant [herbs], and a magnificent [apple tree] stands in the center, its branches heavy with ripe red fruit. The village elder tends to this garden with great care."
               '((:north . village-square))
               '(("herbs" . "Lavender, rosemary, and thyme fill the air with their sweet scent. The elder uses these in healing remedies.")
                 ("apple tree" . "An ancient apple tree, its gnarled branches reaching skyward. The apples look delicious and perfectly ripe.")))

  ;; Create and place vehicle items in rooms
  (let ((car-item (mud.inventory::make-item
                   :name "car"
                   :type :vehicle
                   :vehicle-type :uber
                   :description "A sleek, magical carriage that shimmers with arcane energy. You can enter it to travel anywhere instantly."))
        (skiff-item (mud.inventory::make-item
                     :name "skiff"
                     :type :vehicle
                     :vehicle-type :water
                     :description "A small wooden boat called 'Eternal Wanderer'. You can enter it to navigate water passages."))
        (apple-item (mud.inventory::make-item
                     :name "apple"
                     :type :consumable
                     :description "A perfectly ripe red apple from the village garden. It looks delicious!")))
    (add-item-to-room 'village-square car-item)
    (add-item-to-room 'riverbank skiff-item)
    (add-item-to-room 'village-garden apple-item)))

(defun find-room (room-id)
  (gethash room-id *rooms*))

(defun find-room-by-name (room-name)
  "Find a room by its name (case-insensitive, partial match supported)"
  (let ((search-name (string-downcase room-name)))
    (loop for room being the hash-values of *rooms*
          when (search search-name (string-downcase (room-name room)))
          return room)))

(defun starting-room ()
  (or (find-room *starting-room*)
      (error "Starting room ~a is undefined." *starting-room*)))

(defun neighbor (room direction &optional vehicle-type)
  "Find the room connected by direction. If vehicle-type is provided, only return exits of that type."
  (let ((exit-entry (assoc direction (room-exits room))))
    (when exit-entry
      (let ((rest-of-entry (cdr exit-entry)))
        ;; Check if this is a typed exit (format: (:direction :type . room))
        ;; A typed exit will have a keyword as the first element of the cdr
        (if (and (consp rest-of-entry) (keywordp (car rest-of-entry)))
            ;; Typed exit - check if vehicle matches
            (when (and vehicle-type (eq vehicle-type (car rest-of-entry)))
              (cdr rest-of-entry))
            ;; Simple exit (format: (:direction . room))
            ;; Accessible when not in a vehicle OR in an uber vehicle
            (when (or (null vehicle-type) (eq vehicle-type :uber))
              rest-of-entry))))))

(defun add-item-to-room (room-id item)
  "Add an item to a room"
  (let ((room (find-room room-id)))
    (when room
      (push item (room-items room)))))

(defun remove-item-from-room (room-id item)
  "Remove a specific item from a room"
  (let ((room (find-room room-id)))
    (when room
      (setf (room-items room)
            (remove item (room-items room) :test #'eq)))))

(defun fuzzy-match-item-name (item-name-full item-name-partial)
  "Check if partial name matches the full item name (supports substring matching)"
  (let ((full (string-downcase item-name-full))
        (partial (string-downcase item-name-partial)))
    (or (string-equal full partial)
        (search partial full))))

(defun find-item-in-room (room-id item-name)
  "Find the first item in a room matching the name (supports partial matches)"
  (let ((room (find-room room-id)))
    (when room
      (find-if (lambda (item)
                 (fuzzy-match-item-name (mud.inventory::item-name item) item-name))
               (room-items room)))))

(defun find-facet-in-room (room-id facet-name)
  "Find a facet in a room by name (supports partial matches)"
  (let ((room (find-room room-id)))
    (when room
      (find-if (lambda (facet-pair)
                 (fuzzy-match-item-name (car facet-pair) facet-name))
               (room-facets room)))))

(defun list-room-items (room-id)
  "Return a formatted list of items in a room"
  (let ((room (find-room room-id)))
    (when room
      (let ((items (room-items room)))
        (if (null items)
            nil
            (let ((item-counts (make-hash-table :test 'equal)))
              ;; Count items by name
              (dolist (item items)
                (incf (gethash (mud.inventory::item-name item) item-counts 0)))
              ;; Build string with counts
              (with-output-to-string (out)
                (maphash (lambda (name count)
                           (format out "~a~a"
                                  (if (> count 1)
                                      (format nil "~a (x~d)" name count)
                                      name)
                                  (if (> (hash-table-count item-counts) 1) ", " "")))
                         item-counts))))))))

(defun generate-map (current-room-id)
  "Generate an ASCII map of the world with the player's current location marked"
  (with-output-to-string (s)
    (format s "~%")
    (format s "  [Graveyard]          [Tavern Loft]~%")
    (format s "       ~a                    ~a~%"
            (if (eq current-room-id 'graveyard) "*" " ")
            (if (eq current-room-id 'tavern-loft) "*" " "))
    (format s "        \\                   |~%")
    (format s "         \\           [Bronze Badger]~%")
    (format s "          \\                 ~a~%"
            (if (eq current-room-id 'tavern-common-room) "*" " "))
    (format s "           \\                |~%")
    (format s "  [Market]-----[Village Square]-----[Moonlit Lane]-----[Whispering Wood]~%")
    (format s "      ~a               ~a                  ~a                   ~a~%"
            (if (eq current-room-id 'market-stalls) "*" " ")
            (if (eq current-room-id 'village-square) "*" " ")
            (if (eq current-room-id 'moonlit-lane) "*" " ")
            (if (eq current-room-id 'whispering-wood) "*" " "))
    (format s "      |~%")
    (format s "  [Riverbank]~%")
    (format s "      ~a~%"
            (if (eq current-room-id 'riverbank) "*" " "))
    (format s "      |~%")
    (format s "      | (enter skiff)~%")
    (format s "      |~%")
    (format s "  [Hidden Cove]~%")
    (format s "      ~a~%"
            (if (eq current-room-id 'hidden-cove) "*" " "))
    (format s "~%")
    (format s "  * = Your current location~%")))
