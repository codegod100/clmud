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
  (define-vehicle "ufo" :air "A shimmering disc of otherworldly metal that defies gravity. It hums with cosmic energy." :damage 50 :speed 20)

  (define-room 'village-square
               "Village Square"
               "Cobblestone paths converge beneath an [ancient oak], its leaves whispering tales of heroes past. [Lanterns] sway gently, casting golden halos in the dusk."
               '((:north :pedestrian . tavern-common-room)
                 (:east . moonlit-lane)
                 (:south :pedestrian . village-garden)
                 (:west . market-stalls)
                 (:northwest . graveyard)
                 (:up :air . sky-over-village))
               '(("ancient oak" . "The massive oak has stood here for centuries. Carved into its trunk are names of heroes long past, and a small hollow near the base seems to hide something...")
                 ("lanterns" . "The lanterns are wrought iron, their flames never seeming to dim. They were said to be blessed by an ancient mage to guide lost travelers.")))
  (define-room 'tavern-common-room
               "The Bronze Badger"
               "Warm lamplight spills over polished oak tables, while the scent of spiced cider mingles with distant lute music. A crackling [hearth] invites weary travelers. Behind the bar, a [weathered map] hangs on the wall."
               '((:south :pedestrian . village-square)
                 (:up :pedestrian . tavern-loft))
               '(("hearth" . "The stone hearth crackles with eternal flame. Local legend says it was lit by the first settlers and has never gone out.")
                 ("weathered map" . "The map shows the surrounding lands, with several locations marked with red X's. One appears to be deep in the Whispering Wood...")))
  (define-room 'tavern-loft
               "Tavern Loft"
               "Low beams and soft straw mattresses offer respite. A narrow [window] reveals the silver glow of the moonlit treeline beyond the village walls."
               '((:down :pedestrian . tavern-common-room))
               '(("window" . "Through the dusty glass, you can see the distant forest. Something occasionally glimmers between the trees...")))
  (define-room 'moonlit-lane
               "Moonlit Lane"
               "A narrow lane stretches eastward, flanked by [ivy-clad cottages]. Fireflies dance in the night air, drawing the eye toward the shadowed [forest archway]."
               '((:west . village-square)
                 (:east . whispering-wood)
                 (:up :air . sky-over-village))
               '(("ivy-clad cottages" . "The cottages are ancient, their windows dark. The ivy seems to move slightly, as if breathing...")
                 ("forest archway" . "Two twisted trees form a natural archway leading into the Whispering Wood. Strange runes are carved into their bark.")))
  (define-room 'whispering-wood
               "Whispering Wood"
               "Towering pines murmur secrets overhead, their needles shimmering with dew. Somewhere deeper within, an [owl] hoots, beckoning the brave. A [standing stone] covered in moss rises from the forest floor."
               '((:west . moonlit-lane)
                 (:up :air . sky-over-forest))
               '(("owl" . "The owl watches you with knowing eyes. It seems to be guarding something in the deeper woods...")
                 ("standing stone" . "Ancient runes cover the stone. You can barely make out the words: 'Only the worthy may pass beyond...'")))
  (define-room 'market-stalls
               "Closing Market"
               "Canopies ripple in the breeze as merchants shutter their stalls. The lingering aroma of [roasted chestnuts] and fresh parchment fills the air. A [notice board] displays various announcements."
               '((:east . village-square)
                 (:south . riverbank)
                 (:up :air . sky-over-market))
               '(("roasted chestnuts" . "The warm chestnuts are a local specialty. The vendor mentions that the recipe is a closely guarded secret passed down for generations.")
                 ("notice board" . "Several notices are pinned here: 'WANTED: Brave souls to investigate disappearances in Whispering Wood. Reward offered.' Another reads: 'Lost: Family heirloom ring, last seen near the riverbank.'")))
  (define-room 'riverbank
               "Riverbank"
               "Moonlight paints the [river] in silver ribbons. A wooden skiff knocks gently against the pier, ready for anyone bold enough to cast off."
               '((:north . market-stalls)
                 (:downstream :water . hidden-cove)
                 (:up :air . sky-over-river))
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
               '((:southeast . village-square)
                 (:up :air . sky-over-graveyard))
               '(("tombstones" . "Most inscriptions are illegible, but one reads: 'Here lies the Keeper of Secrets. Death is not the end, merely a door.' Fresh flowers rest at its base, though no one living has been seen placing them.")
                 ("ethereal glow" . "The glow pulses gently, like a heartbeat. Those who have died and returned speak of a presence here - neither malevolent nor kind, simply... waiting.")))

  (define-room 'village-garden
               "Village Garden"
               "A small, peaceful garden bursting with life. Rows of vegetables grow alongside fragrant [herbs], and a magnificent [apple tree] stands in the center, its branches heavy with ripe red fruit. The village elder tends to this garden with great care."
               '((:north :pedestrian . village-square))
               '(("herbs" . "Lavender, rosemary, and thyme fill the air with their sweet scent. The elder uses these in healing remedies.")
                 ("apple tree" . "An ancient apple tree, its gnarled branches reaching skyward. The apples look delicious and perfectly ripe.")))

  ;; Sky rooms - aerial views
  (define-room 'sky-over-village
               "Sky Above the Village"
               "You soar high above the village, the UFO humming beneath you. Below, the [village square] is a small cobblestone circle, the [tavern] a warm glow, and the [market] a cluster of colorful canopies. To the east, dark [woods] stretch like a shadowy blanket. To the northwest, the [graveyard] glows with ethereal light."
               '((:down :air . village-square)
                 (:east . sky-over-forest)
                 (:west . sky-over-market)
                 (:northwest . sky-over-graveyard))
               '(("village square" . "From up here, you can see the ancient oak tree in the center, its branches spreading like welcoming arms. Tiny figures move through the square like ants.")
                 ("tavern" . "The Bronze Badger's chimney puffs smoke into the evening air. You can almost smell the spiced cider from here.")
                 ("market" . "The market stalls look like a patchwork quilt from this height, merchants closing up for the night.")
                 ("woods" . "The Whispering Wood is a sea of pine trees, their tops swaying in unison. Something ancient dwells there.")
                 ("graveyard" . "The graveyard is veiled in mist even from this height, its ethereal glow pulsing like a beacon in the darkness.")))

  (define-room 'sky-over-forest
               "Sky Above the Whispering Wood"
               "The UFO glides silently over the vast expanse of the [Whispering Wood]. The pine canopy below ripples like dark green waves. You can see a [clearing] and what looks like a [standing stone] poking through the trees. To the west, the village lights twinkle."
               '((:down :air . whispering-wood)
                 (:west . sky-over-village))
               '(("whispering wood" . "The forest stretches endlessly, ancient and mysterious. Even from above, you feel its secretive nature.")
                 ("clearing" . "A small clearing in the forest where moonlight breaks through. You glimpse movement - perhaps the wolf or goblin you've heard about.")
                 ("standing stone" . "The ancient stone juts up through the canopy, covered in glowing runes that pulse even from this height.")))

  (define-room 'sky-over-market
               "Sky Above the Market"
               "You hover above the [market district], watching the last merchants pack their wares. The [riverbank] glitters to the south where moonlight dances on water. The village square lies to the east, and beyond it, darkness."
               '((:down :air . market-stalls)
                 (:east . sky-over-village)
                 (:south . sky-over-river))
               '(("market district" . "The market is a maze of stalls and canopies from above. You can see the notice board and smell the faint aroma of roasted chestnuts.")
                 ("riverbank" . "The river winds like a silver serpent through the land, reflecting moonlight. A small dock juts out with something moored to it.")))

  (define-room 'sky-over-river
               "Sky Above the River"
               "The UFO drifts above the winding [river], its waters shimmering below. You can trace its path downstream to a [hidden cove] surrounded by sheer cliffs. To the north, the market district is visible."
               '((:down :air . riverbank)
                 (:north . sky-over-market))
               '(("river" . "The river flows peacefully below, its surface mirror-smooth except where it churns around rocks.")
                 ("hidden cove" . "A secluded cove nestled between towering cliffs. From here, you can see ancient ruins carved into the cliff face, glowing faintly.")))

  (define-room 'sky-over-graveyard
               "Sky Above the Graveyard"
               "The UFO hovers silently above the ancient [graveyard]. From this height, the tombstones look like scattered bones, and the [ethereal mist] glows eerily in the moonlight. To the southeast, the village square is visible."
               '((:down :air . graveyard)
                 (:southeast . sky-over-village))
               '(("graveyard" . "The graveyard is shrouded in perpetual mist, even from above. The ethereal glow pulses rhythmically, like a heartbeat visible from the heavens.")
                 ("ethereal mist" . "The mist swirls in patterns that seem almost deliberate, forming shapes that vanish when you try to focus on them.")))

  ;; Create and place vehicle items in rooms
  (let ((car-item (mud.inventory::make-item
                   :name "car"
                   :type :vehicle
                   :vehicle-type :uber
                   :portable nil
                   :description "A sleek, magical carriage that shimmers with arcane energy. You can enter it to travel anywhere instantly."))
        (skiff-item (mud.inventory::make-item
                     :name "skiff"
                     :type :vehicle
                     :vehicle-type :water
                     :portable nil
                     :description "A small wooden boat called 'Eternal Wanderer'. You can enter it to navigate water passages."))
        (ufo-item (mud.inventory::make-item
                   :name "ufo"
                   :type :vehicle
                   :vehicle-type :air
                   :portable nil
                   :description "A gleaming silver disc that hovers silently above the ground, pulsing with otherworldly energy. Strange symbols glow along its edge."))
        (apple-item (mud.inventory::make-item
                     :name "apple"
                     :type :consumable
                     :description "A perfectly ripe red apple from the village garden. It looks delicious!")))
    (add-item-to-room 'village-square car-item)
    (add-item-to-room 'riverbank skiff-item)
    (add-item-to-room 'graveyard ufo-item)
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
            ;; Typed exit - check exit type
            (let ((exit-type (car rest-of-entry)))
              (cond
                ;; Pedestrian/indoor exits - only accessible on foot (no vehicle)
                ((eq exit-type :pedestrian)
                 (when (null vehicle-type)
                   (cdr rest-of-entry)))
                ;; Water exits - only accessible with water vehicles
                ((eq exit-type :water)
                 (when (and vehicle-type (eq vehicle-type :water))
                   (cdr rest-of-entry)))
                ;; Air/sky exits - only accessible with air vehicles
                ((eq exit-type :air)
                 (when (and vehicle-type (eq vehicle-type :air))
                   (cdr rest-of-entry)))
                ;; Other typed exits - vehicle must match
                (t
                 (when (and vehicle-type (eq vehicle-type exit-type))
                   (cdr rest-of-entry)))))
            ;; Simple exit (format: (:direction . room))
            ;; Accessible when not in a vehicle OR in an uber/air vehicle
            (when (or (null vehicle-type) (eq vehicle-type :uber) (eq vehicle-type :air))
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
