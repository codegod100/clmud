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

(eval-when (:compile-toplevel :load-toplevel :execute)
  (declaim (ftype (function (t t) *) add-item-to-room))
  (declaim (ftype (function (t t) *) remove-item-from-room))
  (declaim (ftype (function (t t) *) find-item-in-room)))

;;; Vehicle definitions
(defstruct vehicle
  name
  type
  description
  (damage 0 :type integer)     ; Damage when ramming
  (speed 0 :type integer)      ; Speed/momentum bonus
  (armor 0 :type integer)      ; Armor rating - absorbs damage until exceeded
  (max-armor 0 :type integer)) ; Maximum armor (for repair purposes)

(defparameter *vehicles* (make-hash-table :test 'equal)
  "Hash table of vehicle name -> vehicle struct")

(defun define-vehicle (name type description &key (damage 0) (speed 0) (armor 0))
  "Define a vehicle that can be entered"
  (setf (gethash name *vehicles*)
        (make-vehicle :name name :type type :description description
                      :damage damage :speed speed :armor armor :max-armor armor)))

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
  (define-vehicle "skiff" :water "A small wooden boat that glides across water on its own." :armor 5)
  (define-vehicle "boat" :water "A small wooden boat that glides across water on its own." :damage 5 :speed 2 :armor 8)
  (define-vehicle "eternal wanderer" :water "A small wooden boat that glides across water on its own." :damage 5 :speed 2 :armor 8)
  (define-vehicle "car" :uber "A sleek, magical carriage that can take you anywhere instantly." :damage 30 :speed 10 :armor 25)
  (define-vehicle "carriage" :uber "A sleek, magical carriage that can take you anywhere instantly." :damage 30 :speed 10 :armor 25)
  (define-vehicle "ufo" :air "A shimmering disc of otherworldly metal that defies gravity. It hums with cosmic energy." :damage 50 :speed 20 :armor 40)

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
                 (:east . ancient-grove)
                 (:up :air . sky-over-forest))
               '(("owl" . "The owl watches you with knowing eyes. It seems to be guarding something in the deeper woods...")
                 ("standing stone" . "Ancient runes cover the stone. You can barely make out the words: 'Only the worthy may pass beyond...'")))
  (define-room 'ancient-grove
               "Ancient Grove"
               "Moonlight spills into this hidden grove, illuminating a ring of [standing stones]. The air hums with primal power, and deep claw marks score the surrounding trees. A hush falls here -- the domain of the Forest Guardian."
               '((:west . whispering-wood))
               '(("standing stones" . "Each stone bears the sigil of a guardian spirit. The runes glow brighter as you approach, as if judging your resolve.")
                 ("claw marks" . "Massive claw marks gouge the bark. Whatever prowls this grove is far larger than the wolves near the village.")))
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
                 (:south :water . hidden-cove)
                 (:up :air . sky-over-river))
               '(("river" . "The water flows swiftly, its depths dark and mysterious. You think you see something glinting on the riverbed...")))
  (define-room 'hidden-cove
               "Hidden Cove"
               "A secluded cove surrounded by towering cliffs. The water is calm here, crystal clear. Ancient [ruins] peek through the vines on the cliff face, and a narrow [cave entrance] yawns in the rock. A weathered [shipwreck] lies half-submerged near the shore."
               '((:north :water . riverbank))
               '(("ruins" . "The ruins are covered in the same strange runes you saw on the forest archway. They seem to glow faintly in the moonlight, pulsing with an otherworldly energy.")
                 ("cave entrance" . "The cave is dark and foreboding. Cold air wafts out, carrying whispers of things best left undisturbed. You sense great power - and great danger - within.")
                 ("shipwreck" . "The remains of a once-proud vessel, now broken and weathered by time. Barnacles cling to its hull, and seaweed drapes from its broken masts.")))
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
               "The UFO drifts above the winding [river], its waters shimmering below. You can trace its path south to a [hidden cove] surrounded by sheer cliffs. To the north, the market district is visible."
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
                     :quest-item t
                     :description "A perfectly ripe red apple from the village garden. It looks delicious!"))
        (treasure-map-item (mud.inventory::make-item
                            :name "treasure-map"
                            :type :quest-item
                            :quest-item t
                            :description "An old, weathered treasure map showing the location of Captain Blackbeard's buried gold. The map is marked with an 'X' near the hidden cove, but the exact spot seems to be in the ancient grove.")))
    (add-item-to-room 'village-square car-item)
    (add-item-to-room 'riverbank skiff-item)
    (add-item-to-room 'graveyard ufo-item)
    (add-item-to-room 'village-garden apple-item)
    (add-item-to-room 'ancient-grove treasure-map-item)))

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

(defun direction-delta (direction)
  "Return (dx dy) offset for a given direction"
  (case direction
    (:north '(0 -1))
    (:south '(0 1))
    (:east '(1 0))
    (:west '(-1 0))
    (:northeast '(1 -1))
    (:northwest '(-1 -1))
    (:southeast '(1 1))
    (:southwest '(-1 1))
    (:up '(0 0))      ; Up/down don't affect 2D map
    (:down '(0 0))
    (:north '(0 -1))
    (:south '(0 1))
    (t '(0 0))))

(defun explore-map (start-room-id &optional (vehicle-type nil) (max-depth 20))
  "Explore rooms from start position and build a coordinate map.
   When vehicle-type is :air, shows sky rooms. Otherwise shows ground rooms."
  (declare (ignorable max-depth))
  (let ((coords (make-hash-table :test #'eq))
        (visited (make-hash-table :test #'eq))
        (queue (list (list start-room-id 0 0))))
    (setf (gethash start-room-id coords) '(0 0))
    (setf (gethash start-room-id visited) t)

    (loop while queue do
      (let* ((current (pop queue))
             (room-id (first current))
             (x (second current))
             (y (third current))
             (room (find-room room-id)))

        (when room
          (dolist (exit (room-exits room))
            (let* ((dir (first exit))
                   ;; Handle exit formats: (:dir . dest) or (:dir :type . dest)
                   (rest-of-exit (cdr exit))
                   ;; Check if second element is a keyword (movement type)
                   (has-movement-type (and (consp rest-of-exit) (keywordp (car rest-of-exit))))
                   (movement-type (when has-movement-type (car rest-of-exit)))
                   (dest-id (if has-movement-type
                               (cdr rest-of-exit)  ; (:dir :type . dest)
                               rest-of-exit))      ; (:dir . dest)
                   (delta (direction-delta dir))
                   (new-x (+ x (first delta)))
                   (new-y (+ y (second delta))))

              ;; For air vehicles, show sky rooms. For ground, show ground rooms.
              (let ((is-sky-room (and (symbolp dest-id)
                                      (search "SKY" (symbol-name dest-id)))))
              ;; Only add room if:
              ;; 1. Not already visited
              ;; 2. Vertical moves allowed only for air vehicles
              ;; 3. Movement type matches vehicle (or no restriction)
              ;; 4. Ground traversal skips sky rooms unless in an air vehicle
              (unless (or (gethash dest-id visited)
                          (and (member dir '(:up :down))
                               (not (eq vehicle-type :air)))
                          ;; Check movement type compatibility
                          (and has-movement-type movement-type vehicle-type
                               (not (eq movement-type vehicle-type))
                               (not (eq vehicle-type :air))  ; Air can go anywhere
                               (not (eq vehicle-type :uber))) ; Uber can go anywhere
                          ;; Skip sky rooms unless we are flying
                          (and (not (eq vehicle-type :air))
                               is-sky-room))
                  (setf (gethash dest-id visited) t)
                  (setf (gethash dest-id coords) (list new-x new-y))
                  (push (list dest-id new-x new-y) queue))))))))
    coords))

(defun get-map-bounds (coords)
  "Get min/max x/y from coordinate hash"
  (let ((min-x 0) (max-x 0) (min-y 0) (max-y 0))
    (maphash (lambda (room-id pos)
               (declare (ignore room-id))
               (let ((x (first pos))
                     (y (second pos)))
                 (setf min-x (min min-x x))
                 (setf max-x (max max-x x))
                 (setf min-y (min min-y y))
                 (setf max-y (max max-y y))))
             coords)
    (list min-x max-x min-y max-y)))

(defun abbreviate-room-name (room-id)
  "Create a short abbreviation for a room name with ASCII symbols"
  (let ((room (find-room room-id)))
    (if room
        (let ((name (room-name room)))
          (cond
            ;; Special cases with ASCII symbols
            ((string= name "Village Square") "[V] Village")
            ((string= name "The Bronze Badger") "[T] Tavern")
            ((string= name "Tavern Loft") "[L] Loft")
            ((string= name "Whispering Wood") "[F] Forest")
            ((string= name "Moonlit Lane") "[R] Lane")
            ((string= name "Closing Market") "[M] Market")
            ((string= name "Riverbank") "[~] River")
            ((string= name "Hidden Cove") "[C] Cove")
            ((string= name "Graveyard") "[G] Grave")
            ((string= name "Village Garden") "[H] Garden")
            ((string= name "Ancient Grove") "[A] Grove")
            ;; Sky rooms
            ((string= name "Sky Above the Village") "[^] SkyVill")
            ((string= name "Sky Above the Whispering Wood") "[^] SkyWood")
            ((string= name "Sky Above the Market") "[^] SkyMrkt")
            ((string= name "Sky Above the River") "[^] SkyRivr")
            ((string= name "Sky Above the Graveyard") "[^] SkyGrvy")
            ;; Default: take first word with a generic symbol
            (t (let ((first-word (subseq name 0 (or (position #\Space name) (length name)))))
                 (format nil "[*] ~a" (subseq first-word 0 (min 6 (length first-word))))))))
        "[?] ???")))

(defun generate-map (current-room-id &optional (vehicle-type nil))
  "Render an ASCII map centred on CURRENT-ROOM-ID. When VEHICLE-TYPE is :air,
   only sky rooms are shown; otherwise ground-level rooms are displayed."
  (let ((coords (or (explore-map current-room-id vehicle-type)
                    (make-hash-table :test #'eq))))
    (if (zerop (hash-table-count coords))
        ""
        (destructuring-bind (min-x max-x min-y max-y) (get-map-bounds coords)
          (let* ((width (* (+ (- max-x min-x) 1) 2))
                 (height (* (+ (- max-y min-y) 1) 2))
                 (grid (make-array (list height width) :initial-element nil)))

            ;; Place each discovered room on the grid.
            (loop for room-id being the hash-keys of coords
                  using (hash-value pos) do
                  (let ((x (* (- (first pos) min-x) 2))
                        (y (* (- (second pos) min-y) 2)))
                    (let ((existing (aref grid y x)))
                      (cond
                        ((null existing)
                         (setf (aref grid y x) (list room-id)))
                        ((listp existing)
                         (pushnew room-id existing :test #'eq)
                         (setf (aref grid y x) existing))
                        ((symbolp existing)
                         (if (eq existing room-id)
                             (setf (aref grid y x) (list room-id))
                             (setf (aref grid y x) (list existing room-id))))
                        (t
                         (setf (aref grid y x) (list room-id)))))))

            ;; Draw connectors so navigation routes are visible with aesthetic symbols
            (loop for room-id being the hash-keys of coords
                  using (hash-value pos) do
                  (let* ((x (* (- (first pos) min-x) 2))
                         (y (* (- (second pos) min-y) 2))
                         (room (find-room room-id)))
                    (when room
                      (dolist (exit (room-exits room))
                        (let* ((dir (first exit))
                               (rest (cdr exit))
                               (dest-id (if (and (consp rest) (keywordp (car rest)))
                                            (cdr rest)
                                            rest))
                               (exit-type (when (and (consp rest) (keywordp (car rest)))
                                           (car rest))))
                          (when (and (gethash dest-id coords)
                                     (not (member dir '(:up :down))))
                            (let* ((delta (direction-delta dir))
                                   (conn-x (+ x (first delta)))
                                   (conn-y (+ y (second delta))))
                              (when (and (not (equal delta '(0 0)))
                                         (<= 0 conn-x) (< conn-x width)
                                         (<= 0 conn-y) (< conn-y height))
                                (setf (aref grid conn-y conn-x)
                                      (case dir
                                        ((:north :south) 
                                         (if (eq exit-type :water) :conn-water-vertical
                                             (if (eq exit-type :air) :conn-air-vertical
                                                 :conn-vertical)))
                                        ((:east :west) 
                                         (if (eq exit-type :water) :conn-water-horizontal
                                             (if (eq exit-type :air) :conn-air-horizontal
                                                 :conn-horizontal)))
                                        ((:northeast :southwest) 
                                         (if (eq exit-type :water) :conn-water-ne-sw
                                             (if (eq exit-type :air) :conn-air-ne-sw
                                                 :conn-ne-sw)))
                                        ((:northwest :southeast) 
                                         (if (eq exit-type :water) :conn-water-nw-se
                                             (if (eq exit-type :air) :conn-air-nw-se
                                                 :conn-nw-se)))
                                        (t :conn-unknown))))))))))

            ;; Compute renderable strings per cell and track column widths.
            (let ((col-widths (make-array width :initial-element 3))
                  (cell-strings (make-array (list height width) :initial-element "")))
              (loop for y from 0 below height do
                (loop for x from 0 below width do
                  (let* ((cell (aref grid y x))
                         (text (cond
                                 ;; Enhanced connector symbols (ASCII)
                                 ((eq cell :conn-horizontal) "===")
                                 ((eq cell :conn-vertical) " | ")
                                 ((eq cell :conn-ne-sw) " / ")
                                 ((eq cell :conn-nw-se) " \\ ")
                                 ;; Water connectors
                                 ((eq cell :conn-water-horizontal) "---")
                                 ((eq cell :conn-water-vertical) " - ")
                                 ((eq cell :conn-water-ne-sw) " - ")
                                 ((eq cell :conn-water-nw-se) " - ")
                                 ;; Air connectors
                                 ((eq cell :conn-air-horizontal) "...")
                                 ((eq cell :conn-air-vertical) " . ")
                                 ((eq cell :conn-air-ne-sw) " . ")
                                 ((eq cell :conn-air-nw-se) " . ")
                                 ;; Room cells with enhanced formatting
                                 ((listp cell)
                                  (let* ((rooms (if (member current-room-id cell :test #'eq)
                                                    (cons current-room-id
                                                          (remove current-room-id cell :test #'eq))
                                                    cell))
                                         (labels (mapcar
                                                  (lambda (rid)
                                                    (let* ((room (find-room rid))
                                                           (abbr (if room
                                                                     (abbreviate-room-name rid)
                                                                     (symbol-name rid)))
                                                           (marker (if (eq rid current-room-id) " *" "")))
                                                      (format nil "~a~a" abbr marker)))
                                                  rooms)))
                                    (format nil "+-~{~a~^-+-~}-+" labels)))
                                 ((symbolp cell)
                                  (let ((room (find-room cell)))
                                    (if room
                                        (format nil "+-~a~a-+"
                                                (abbreviate-room-name cell)
                                                (if (eq cell current-room-id) " *" ""))
                                        "")))
                                 (t "   "))))
                    (setf (aref cell-strings y x) text)
                    (setf (aref col-widths x)
                          (max (aref col-widths x) (max 3 (length text)))))))

              (let ((rendered
                     (with-output-to-string (s)
                       ;; Header with ASCII art
                       (format s "~%")
                       (format s "    +=========================================+~%")
                       (format s "    |              WORLD MAP                  |~%")
                       (format s "    +=========================================+~%")
                       (format s "~%")
                       ;; Map content
                       (loop for y from 0 below height do
                         (loop for x from 0 below width do
                           (format s "~v<~a~>" (aref col-widths x)
                                   (aref cell-strings y x)))
                         (format s "~%"))
                       ;; Footer with legend
                       (format s "~%")
                       (format s "    +=========================================+~%")
                       (format s "    | LEGEND:                                 |~%")
                       (format s "    |   * = Your current location             |~%")
                       (format s "    |   === = Regular paths                   |~%")
                       (format s "    |   --- = Water routes (need boat)        |~%")
                       (format s "    |   ... = Air routes (need flying vehicle)|~%")
                       (format s "    |   [V] = Village, [T] = Tavern, etc.     |~%")
                       (format s "    +=========================================+~%"))))
                (return-from generate-map rendered)))))))))

(defun generate-artistic-map (current-room-id &optional (vehicle-type nil))
  "Generate a simple artistic map showing the game world layout"
  (let ((coords (or (explore-map current-room-id vehicle-type)
                    (make-hash-table :test #'eq))))
    (if (zerop (hash-table-count coords))
        "The world is shrouded in mystery..."
        (with-output-to-string (s)
          (format s "~%")
          (format s "    +==============================================================+~%")
          (format s "    |                        WORLD MAP                            |~%")
          (format s "    +==============================================================+~%")
          (format s "    |                                                              |~%")
          
          ;; Create a simple artistic representation
          (let ((rooms (loop for room-id being the hash-keys of coords collect room-id)))
            (format s "    |  [V] Village Square (center of the realm)                  |~%")
            (format s "    |      |                                                      |~%")
            (format s "    |      +-- [M] Market Stalls (trading hub)                   |~%")
            (format s "    |      |                                                      |~%")
            (format s "    |      +-- [T] The Bronze Badger (tavern)                    |~%")
            (format s "    |      |   |                                                  |~%")
            (format s "    |      |   +-- [L] Tavern Loft (resting place)               |~%")
            (format s "    |      |                                                      |~%")
            (format s "    |      +-- [F] Whispering Wood (mystical forest)             |~%")
            (format s "    |      |                                                      |~%")
            (format s "    |      +-- [R] Moonlit Lane (dangerous road)                 |~%")
            (format s "    |      |                                                      |~%")
            (format s "    |      +-- [W] Riverbank (water access)                      |~%")
            (format s "    |      |   |                                                  |~%")
            (format s "    |      |   +-- [C] Hidden Cove (secret location)             |~%")
            (format s "    |      |                                                      |~%")
            (format s "    |      +-- [G] Graveyard (undead territory)                  |~%")
            (format s "    |      |                                                      |~%")
            (format s "    |      +-- [H] Village Garden (peaceful retreat)             |~%")
            (format s "    |                                                              |~%")
            (format s "    |  Sky Routes (accessible by air vehicles):                   |~%")
            (format s "    |      * Sky Above Village    * Sky Above Market              |~%")
            (format s "    |      * Sky Above Forest     * Sky Above River               |~%")
            (format s "    |      * Sky Above Graveyard                                  |~%")
            (format s "    |                                                              |~%")
            (format s "    |  Vehicles: Car (instant travel), Skiff (water), UFO (air)   |~%")
            (format s "    |                                                              |~%")
            (format s "    |  Legend: [V]=Village [M]=Market [T]=Tavern [F]=Forest       |~%")
            (format s "    |          [R]=Road [W]=Water [G]=Graveyard [H]=Garden        |~%")
            (format s "    |          [L]=Loft [C]=Cove *=Sky Routes                     |~%"))
          
          (format s "    |                                                              |~%")
          (format s "    +==============================================================+~%")
          (format s "~%")))))

