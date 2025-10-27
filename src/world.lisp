(in-package :mud.world)

(defstruct room
  id
  name
  description
  exits
  (items nil :type list)
  (facets nil :type list)  ; List of (name . description) pairs for examinable details
  (time-descriptions nil :type list)  ; List of (time-of-day . description) pairs for time-based descriptions
  (time-names nil :type list))  ; List of (time-of-day . name) pairs for time-based names

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

;;; Corpse System
(defstruct corpse
  mob-name        ; Name of the mob that died
  items           ; List of items in the corpse
  created-time    ; When the corpse was created
  room-id)        ; Which room the corpse is in

(defparameter *room-corpses* (make-hash-table :test #'eq)
  "Hash table of room-id -> list of corpses in that room")

(defun create-corpse (mob-name items room-id)
  "Create a new corpse with the given items"
  (make-corpse :mob-name mob-name
               :items items
               :created-time (get-universal-time)
               :room-id room-id))

(defun add-corpse-to-room (room-id corpse)
  "Add a corpse to a room"
  (push corpse (gethash room-id *room-corpses*)))

(defun get-corpses-in-room (room-id)
  "Get list of corpses in a room"
  (gethash room-id *room-corpses*))

(defun find-corpse-in-room (room-id corpse-name)
  "Find a corpse in a room by name (case-insensitive, partial match)"
  (let ((search-name (string-downcase corpse-name))
        (corpses (get-corpses-in-room room-id)))
    (find-if (lambda (corpse)
               (search search-name (string-downcase (corpse-mob-name corpse))))
             corpses)))

(defun remove-corpse-from-room (room-id corpse)
  "Remove a corpse from a room"
  (let ((remaining-corpses (remove corpse (gethash room-id *room-corpses*) :test #'eq)))
    (setf (gethash room-id *room-corpses*)
          (if (listp remaining-corpses) remaining-corpses nil))))

(defun loot-corpse (corpse)
  "Get all items from a corpse and return them"
  (let ((items (corpse-items corpse)))
    (setf (corpse-items corpse) nil) ; Clear the corpse
    items))

;;; World Time System
(defparameter *world-time* 0.0
  "Current world time in hours (0.0 = sunrise, 12.0 = sunset)")
(defparameter *day-duration* 20.0
  "Duration of a full day cycle in minutes")
(defparameter *sunrise-time* 6.0
  "Time of sunrise in hours")
(defparameter *sunset-time* 18.0
  "Time of sunset in hours")

;;; Global Tick System
(defparameter *global-tick* 0
  "Global tick counter - increments every game tick")
(defparameter *tick-rate* 12
  "Number of ticks per hour of game time")

(defun get-world-time ()
  "Get current world time in hours"
  *world-time*)

(defun set-world-time (time)
  "Set world time in hours (0.0-24.0)"
  (setf *world-time* (mod time 24.0)))

(defun advance-world-time (minutes)
  "Advance world time by specified minutes"
  (setf *world-time* (mod (+ *world-time* (/ minutes 60.0)) 24.0)))

(defun get-global-tick ()
  "Get current global tick number"
  *global-tick*)

(defun advance-global-tick ()
  "Advance the global tick counter and update world time"
  (incf *global-tick*)
  ;; Advance world time based on tick rate (12 ticks = 1 hour)
  (advance-world-time (/ 60.0 *tick-rate*)))

(defun get-tick-rate ()
  "Get the current tick rate (ticks per hour)"
  *tick-rate*)

(defun set-tick-rate (rate)
  "Set the tick rate (ticks per hour)"
  (setf *tick-rate* rate))

(defun get-time-of-day ()
  "Get time of day as keyword (:dawn, :morning, :afternoon, :evening, :night)"
  (let ((time *world-time*))
    (cond
      ((< time 6.0) :night)
      ((< time 9.0) :dawn)
      ((< time 12.0) :morning)
      ((< time 15.0) :afternoon)
      ((< time 18.0) :evening)
      ((< time 21.0) :dusk)
      (t :night))))

(defun format-world-time ()
  "Format world time as readable string"
  (let* ((hours (floor *world-time*))
         (minutes (floor (* 60 (mod *world-time* 1.0))))
         (time-of-day (get-time-of-day)))
    (format nil "~2,'0d:~2,'0d (~a) [Tick ~d]" hours minutes time-of-day *global-tick*)))

(defun is-daytime-p ()
  "Check if it's currently daytime"
  (let ((time *world-time*))
    (and (>= time *sunrise-time*) (< time *sunset-time*))))

(defun is-nighttime-p ()
  "Check if it's currently nighttime"
  (not (is-daytime-p)))

(defun get-time-based-description (room)
  "Get room description based on current time of day"
  (let ((time-of-day (get-time-of-day))
        (time-descriptions (room-time-descriptions room)))
    (if time-descriptions
        (let ((time-desc (assoc time-of-day time-descriptions)))
          (if time-desc
              (cdr time-desc)
              (room-description room)))
        (room-description room))))

(defun get-time-based-name (room)
  "Get room name based on current time of day"
  (let ((time-of-day (get-time-of-day))
        (time-names (room-time-names room)))
    (if time-names
        (let ((time-name (assoc time-of-day time-names)))
          (if time-name
              (cdr time-name)
              (room-name room)))
        (room-name room))))

(defun add-time-description (room-id time-of-day description)
  "Add a time-based description to a room"
  (let ((room (find-room room-id)))
    (when room
      (let ((existing (assoc time-of-day (room-time-descriptions room))))
        (if existing
            (setf (cdr existing) description)
            (setf (room-time-descriptions room)
                  (cons (cons time-of-day description)
                        (room-time-descriptions room))))))))

(defun add-time-name (room-id time-of-day name)
  "Add a time-based name to a room"
  (let ((room (find-room room-id)))
    (when room
      (let ((existing (assoc time-of-day (room-time-names room))))
        (if existing
            (setf (cdr existing) name)
            (setf (room-time-names room)
                  (cons (cons time-of-day name)
                        (room-time-names room))))))))

(defun define-room-with-time (id name description exits &optional facets time-descriptions)
  "Define a room with time-based descriptions"
  (setf (gethash id *rooms*)
        (make-room :id id :name name :description description :exits exits 
                   :facets facets :time-descriptions time-descriptions)))

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
  (define-vehicle "car" :ground "A sleek, magical carriage that glides smoothly along the roads." :damage 30 :speed 10 :armor 25)
  (define-vehicle "carriage" :ground "A sleek, magical carriage that glides smoothly along the roads." :damage 30 :speed 10 :armor 25)
  (define-vehicle "ufo" :air "A shimmering disc of otherworldly metal that defies gravity. It hums with cosmic energy." :damage 50 :speed 20 :armor 40)
  (define-vehicle "motorcycle" :ground "A sleek black motorcycle with chrome accents. The engine rumbles with power." :damage 15 :speed 8 :armor 12)

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
               "A narrow lane stretches eastward, flanked by [ivy-clad cottages]. Fireflies dance in the night air, drawing the eye toward the shadowed [forest archway]. A dirt road branches off to the north."
               '((:west . village-square)
                 (:east . whispering-wood)
                 (:up :air . sky-over-village)
                 (:north :ground . highway-north))
               '(("ivy-clad cottages" . "The cottages are ancient, their windows dark. The ivy seems to move slightly, as if breathing...")
                 ("forest archway" . "Two twisted trees form a natural archway leading into the Whispering Wood. Strange runes are carved into their bark.")
                 ("dirt road" . "A well-worn dirt road that leads north, perfect for vehicles. It disappears into the distance between rolling hills.")))
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

  ;; Ground vehicle areas
  (define-room 'highway-north
               "Northern Highway"
               "A wide dirt road stretches north and south, flanked by rolling hills and scattered trees. The road is well-maintained and perfect for vehicles. Dust clouds rise from the occasional passing traveler."
               '((:south :ground . moonlit-lane)
                 (:north :ground . highway-crossroads))
               '(("hills" . "The rolling hills to the east and west are covered in wild grass and dotted with ancient oak trees.")
                 ("dust clouds" . "The dry road kicks up dust when vehicles pass, creating small clouds that drift across the landscape.")))
  (define-room 'highway-crossroads
               "Highway Crossroads"
               "A major intersection where several roads meet. A weathered signpost points in multiple directions, and the ground is worn smooth by countless wheels. This is clearly a well-traveled route for vehicles."
               '((:south :ground . highway-north)
                 (:east :ground . highway-east)
                 (:west :ground . highway-west)
                 (:north :ground . northern-outpost))
               '(("signpost" . "The wooden signpost is weathered but still readable. It points to various destinations: 'Village' to the south, 'Eastern Plains' to the east, 'Western Hills' to the west, and 'Northern Outpost' to the north.")
                 ("worn ground" . "The intersection is heavily traveled, with deep ruts from wagon wheels and vehicle tracks crisscrossing the area.")))

  ;; Faction Regions - 5 new areas with distinct factions

  ;; 1. Northern Outpost - The Royal Guard faction
  (define-room 'northern-outpost
               "Northern Outpost"
               "A fortified military outpost manned by the Royal Guard. Stone walls and watchtowers surround a central courtyard where [soldiers] drill with precision. The [commander's quarters] overlook the training grounds, and a [supply depot] stores weapons and provisions. The Royal Guard maintains order and protects the realm from threats."
               '((:south :ground . highway-crossroads)
                 (:east :ground . eastern-plains)
                 (:west :ground . western-hills)
                 (:north :ground . mountain-pass))
               '(("soldiers" . "The Royal Guard soldiers train with discipline and honor. They wear polished armor and carry well-maintained weapons. Their loyalty to the crown is unwavering.")
                 ("commander's quarters" . "A sturdy building where the outpost commander plans operations and receives reports. The Royal Guard flag flies proudly from its roof.")
                 ("supply depot" . "A well-organized storage facility containing weapons, armor, and supplies. Everything is meticulously catalogued and maintained.")))

  ;; 2. Eastern Plains - The Nomad Tribes faction  
  (define-room 'eastern-plains
               "Eastern Plains"
               "Vast grasslands stretch to the horizon, dotted with [nomad camps] and [ancient stone circles]. The Nomad Tribes roam these lands freely, following the seasons and living in harmony with nature. [Traders] set up temporary markets, and [spirit stones] mark sacred sites. The nomads value freedom and independence above all else."
               '((:west :ground . highway-crossroads)
                 (:north :ground . northern-outpost)
                 (:east :ground . spirit-grove)
                 (:south :ground . southern-desert))
               '(("nomad camps" . "Colorful tents and wagons form temporary settlements. The nomads live simply but with great wisdom, sharing stories around their campfires.")
                 ("ancient stone circles" . "Mysterious stone formations that predate recorded history. The nomads say they were placed by the spirits to guide travelers.")
                 ("traders" . "Merchants from distant lands set up temporary stalls, trading exotic goods and stories from faraway places.")
                 ("spirit stones" . "Sacred stones that pulse with ancient energy. The nomads believe they connect the physical world to the spirit realm.")))

  ;; 3. Western Hills - The Mountain Clans faction
  (define-room 'western-hills
               "Western Hills"
               "Rugged mountains rise majestically, their peaks lost in clouds. The Mountain Clans have carved their homes into the rock faces, creating [cliff dwellings] connected by [rope bridges]. [Mining operations] extract precious metals from deep within the earth, and [clan halls] echo with the sound of hammers on anvils. The clans are known for their craftsmanship and stoic determination."
               '((:east :ground . highway-crossroads)
                 (:north :ground . northern-outpost)
                 (:west :ground . deep-mines)
                 (:south :ground . southern-desert))
               '(("cliff dwellings" . "Homes carved directly into the mountain face, with windows overlooking the valley below. The Mountain Clans have lived here for generations.")
                 ("rope bridges" . "Sturdy bridges connect the various levels of the cliff dwellings. The clans are expert climbers and bridge builders.")
                 ("mining operations" . "Deep tunnels lead into the mountain's heart, where miners extract precious metals and gems. The work is dangerous but rewarding.")
                 ("clan halls" . "Great halls where the Mountain Clans gather for important decisions and celebrations. The walls are adorned with trophies and clan banners.")))

  ;; 4. Southern Desert - The Shadow Cult faction
  (define-room 'southern-desert
               "Southern Desert"
               "Endless sand dunes stretch under a scorching sun, broken only by [ancient ruins] and [oasis settlements]. The Shadow Cult operates from hidden [underground temples], practicing forbidden arts and seeking power through dark rituals. [Sandstorms] frequently sweep across the dunes, and [mysterious obelisks] mark the locations of ancient power. The cult values knowledge and power above all else."
               '((:north :ground . highway-crossroads)
                 (:east :ground . eastern-plains)
                 (:west :ground . western-hills)
                 (:south :ground . forbidden-oasis))
               '(("ancient ruins" . "Crumbling structures that hint at a once-great civilization. The Shadow Cult studies these ruins for lost knowledge and power.")
                 ("oasis settlements" . "Small communities built around precious water sources. The inhabitants are wary of outsiders and keep to themselves.")
                 ("underground temples" . "Hidden beneath the sand, these temples are where the Shadow Cult performs its most secretive rituals and studies forbidden arts.")
                 ("sandstorms" . "Fierce winds whip sand into blinding clouds that can last for days. Only the most experienced desert travelers dare venture out during these storms.")
                 ("mysterious obelisks" . "Tall stone pillars covered in strange symbols that seem to shift and change when viewed from different angles. They pulse with dark energy.")))

  ;; 5. Spirit Grove - The Nature Guardians faction
  (define-room 'spirit-grove
               "Spirit Grove"
               "A mystical forest where ancient trees tower overhead, their branches forming a natural cathedral. The Nature Guardians tend to this sacred place, protecting the [spirit wells] and [ancient groves]. [Druid circles] mark places of power, and [wildlife] moves freely without fear. The guardians believe in the balance of nature and the interconnectedness of all living things."
               '((:west :ground . eastern-plains)
                 (:north :ground . northern-outpost)
                 (:east :ground . crystal-caverns)
                 (:south :ground . forbidden-oasis))
               '(("spirit wells" . "Sacred pools of crystal-clear water that never run dry. The Nature Guardians say these wells are connected to the life force of the world itself.")
                 ("ancient groves" . "Circles of the oldest trees in the forest, where the most powerful nature magic is practiced. The air here hums with natural energy.")
                 ("druid circles" . "Stone circles where the Nature Guardians perform their rituals and commune with the spirits of the forest. The stones are carved with symbols of nature.")
                 ("wildlife" . "Animals of all kinds move freely through the grove, unafraid of human presence. They seem to understand that this is a place of peace and protection.")))

  ;; Additional connecting rooms for the faction regions
  (define-room 'mountain-pass
               "Mountain Pass"
               "A narrow path winding through the mountains, connecting the Northern Outpost to the deeper mountain regions. The air is thin and cold, and [eagles] soar overhead. The Royal Guard maintains a small checkpoint here to monitor traffic."
               '((:south :ground . northern-outpost))
               '(("eagles" . "Majestic birds of prey that nest in the high peaks. They seem to watch over the pass with keen eyes.")))

  (define-room 'deep-mines
               "Deep Mines"
               "A network of tunnels that extend deep into the mountain's heart. The Mountain Clans work here extracting precious metals and gems. The air is thick with the sound of [pickaxes] and the glow of [mining lanterns]."
               '((:east :ground . western-hills))
               '(("pickaxes" . "The rhythmic sound of metal striking stone echoes through the tunnels as miners work to extract precious materials.")
                 ("mining lanterns" . "Oil lamps that provide light in the dark tunnels. They cast dancing shadows on the rough stone walls.")))

  (define-room 'crystal-caverns
               "Crystal Caverns"
               "A hidden network of caves filled with glowing crystals that pulse with natural energy. The Nature Guardians use this place for their most sacred rituals, and the [crystal formations] seem to sing with the voice of the earth itself."
               '((:west :ground . spirit-grove))
               '(("crystal formations" . "Massive crystals that grow from the cave floor and ceiling, each one pulsing with a different color of light. They seem to respond to the presence of those with pure hearts.")))

  (define-room 'forbidden-oasis
               "Forbidden Oasis"
               "A hidden oasis deep in the desert, surrounded by ancient ruins and guarded by the Shadow Cult. The water here is said to have mystical properties, and [dark rituals] are performed under the light of the full moon. Only those with the cult's favor may approach safely."
               '((:north :ground . southern-desert)
                 (:west :ground . spirit-grove))
               '(("dark rituals" . "Strange ceremonies are performed here under the cover of darkness. The air itself seems to crackle with forbidden energy.")
                 ("mystical water" . "The oasis water shimmers with an otherworldly glow, and those who drink it report visions of ancient times and forgotten knowledge.")))

  ;; Create and place vehicle items in rooms
  (let ((car-item (mud.inventory::make-item
                   :name "car"
                   :type :vehicle
                   :vehicle-type :ground
                   :portable nil
                   :description "A sleek, magical carriage that glides smoothly along the roads. You can enter it to travel on ground routes."))
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
    (add-item-to-room 'ancient-grove treasure-map-item))

  ;; Add time-based descriptions to key rooms
  (add-time-description 'village-square :dawn
                        "Cobblestone paths converge beneath an [ancient oak], its leaves glistening with morning dew. [Lanterns] still glow softly as the first rays of sunlight filter through the branches, casting long shadows across the square.")
  
  (add-time-description 'village-square :morning
                        "Cobblestone paths converge beneath an [ancient oak], its leaves rustling in the morning breeze. [Lanterns] have been extinguished as villagers begin their daily activities, the square bustling with life and energy.")
  
  (add-time-description 'village-square :afternoon
                        "Cobblestone paths converge beneath an [ancient oak], its leaves providing welcome shade from the afternoon sun. [Lanterns] stand ready for evening, while merchants and travelers move about their business.")
  
  (add-time-description 'village-square :evening
                        "Cobblestone paths converge beneath an [ancient oak], its leaves whispering tales of heroes past. [Lanterns] begin to glow as the sun sets, casting warm golden halos in the gathering dusk.")
  
  (add-time-description 'village-square :dusk
                        "Cobblestone paths converge beneath an [ancient oak], its leaves silhouetted against the darkening sky. [Lanterns] sway gently, casting dancing shadows as night falls over the village.")
  
  (add-time-description 'village-square :night
                        "Cobblestone paths converge beneath an [ancient oak], its leaves rustling in the cool night air. [Lanterns] burn brightly, their golden light creating pools of warmth in the darkness, while stars twinkle overhead.")

  ;; Village Square time-based names
  (add-time-name 'village-square :dawn "Dawn Square")
  (add-time-name 'village-square :morning "Morning Square")
  (add-time-name 'village-square :afternoon "Village Square")
  (add-time-name 'village-square :evening "Evening Square")
  (add-time-name 'village-square :dusk "Dusk Square")
  (add-time-name 'village-square :night "Moonlit Square")

  (add-time-description 'tavern-common-room :dawn
                        "The tavern is quiet in the early morning, with only the crackling [hearth] providing warmth and light. The scent of fresh bread and brewing coffee begins to fill the air as the innkeeper prepares for the day.")
  
  (add-time-description 'tavern-common-room :morning
                        "Warm lamplight spills over polished oak tables as villagers gather for breakfast. The scent of spiced cider mingles with the aroma of fresh pastries. A crackling [hearth] warms the room while the [weathered map] catches the morning light.")
  
  (add-time-description 'tavern-common-room :afternoon
                        "The tavern is alive with midday activity, warm lamplight illuminating polished oak tables. Travelers share stories over hearty meals while the crackling [hearth] provides a cozy atmosphere. The [weathered map] on the wall shows the day's adventures.")
  
  (add-time-description 'tavern-common-room :evening
                        "Warm lamplight spills over polished oak tables as evening patrons gather. The scent of spiced cider mingles with distant lute music while the crackling [hearth] creates a welcoming glow. The [weathered map] seems to hold secrets in the flickering light.")
  
  (add-time-description 'tavern-common-room :dusk
                        "The tavern grows more intimate as dusk falls, warm lamplight creating pools of golden light on polished oak tables. The crackling [hearth] becomes the heart of the room, while the [weathered map] fades into shadow.")
  
  (add-time-description 'tavern-common-room :night
                        "The tavern is alive with nighttime revelry, warm lamplight dancing on polished oak tables. The crackling [hearth] casts flickering shadows while the [weathered map] seems to glow with mysterious energy in the dim light.")

  ;; Tavern time-based names
  (add-time-name 'tavern-common-room :dawn "The Bronze Badger (Quiet)")
  (add-time-name 'tavern-common-room :morning "The Bronze Badger (Breakfast)")
  (add-time-name 'tavern-common-room :afternoon "The Bronze Badger (Lunch)")
  (add-time-name 'tavern-common-room :evening "The Bronze Badger (Dinner)")
  (add-time-name 'tavern-common-room :dusk "The Bronze Badger (Evening)")
  (add-time-name 'tavern-common-room :night "The Bronze Badger (Night)")

  (add-time-description 'whispering-wood :dawn
                        "Towering pines stand silent in the misty dawn, their needles glistening with morning dew. An [owl] perches on a branch, its eyes reflecting the first light. A [standing stone] emerges from the morning fog, ancient and mysterious.")
  
  (add-time-description 'whispering-wood :morning
                        "Towering pines murmur softly in the morning breeze, their needles shimmering with dew. Sunlight filters through the canopy, creating dancing patterns on the forest floor. An [owl] watches from the shadows while a [standing stone] catches the morning light.")
  
  (add-time-description 'whispering-wood :afternoon
                        "Towering pines provide cool shade from the afternoon sun, their needles rustling in the warm breeze. Dappled sunlight creates a mosaic on the forest floor. An [owl] dozes in the branches while the [standing stone] stands sentinel in the filtered light.")
  
  (add-time-description 'whispering-wood :evening
                        "Towering pines whisper secrets as evening approaches, their needles catching the last golden rays. An [owl] stirs from its daytime rest, preparing for the night. The [standing stone] seems to glow with ancient power as shadows lengthen.")
  
  (add-time-description 'whispering-wood :dusk
                        "Towering pines become dark silhouettes against the fading sky, their needles rustling with evening secrets. An [owl] hoots softly, heralding the coming night. The [standing stone] stands like a guardian in the gathering darkness.")
  
  (add-time-description 'whispering-wood :night
                        "Towering pines murmur secrets overhead, their needles shimmering with starlight. An [owl] hoots mysteriously in the darkness, beckoning the brave. The [standing stone] glows faintly with otherworldly energy, covered in moss and ancient power.")

  (add-time-description 'graveyard :dawn
                        "Ancient [tombstones] emerge from the morning mist, their inscriptions barely visible in the pale light. The air is still and heavy with dew. A faint [ethereal glow] pulses softly, marking the boundary between night and day.")
  
  (add-time-description 'graveyard :morning
                        "Ancient [tombstones] stand in the morning light, their inscriptions worn by time but visible in the gentle rays. The air is fresh and still. A faint [ethereal glow] shimmers softly, a reminder of the mysteries that lie beyond.")
  
  (add-time-description 'graveyard :afternoon
                        "Ancient [tombstones] cast long shadows in the afternoon sun, their inscriptions clear in the bright light. The air is warm and still. A faint [ethereal glow] is barely visible in the daylight, but its presence is felt.")
  
  (add-time-description 'graveyard :evening
                        "Ancient [tombstones] are bathed in golden light as the sun sets, their inscriptions glowing with warmth. The air grows still and heavy. A faint [ethereal glow] begins to strengthen as day turns to night.")
  
  (add-time-description 'graveyard :dusk
                        "Ancient [tombstones] become dark silhouettes against the evening sky, their inscriptions fading into shadow. The air is still and heavy with the weight of countless souls. A faint [ethereal glow] pulses more strongly in the gathering darkness.")
  
  (add-time-description 'graveyard :night
                        "Ancient [tombstones] lean in the mist, their inscriptions worn by time. The air is still, heavy with the weight of countless souls who have passed through this veil. A faint [ethereal glow] marks the boundary between life and death, pulsing with otherworldly energy.")

  ;; Tavern Loft time descriptions
  (add-time-description 'tavern-loft :dawn
                        "The loft is peaceful in the early morning, with only the soft creaking of floorboards below. A narrow [window] reveals the first light of dawn filtering through the village, casting gentle shadows across the straw mattresses.")
  
  (add-time-description 'tavern-loft :morning
                        "Morning light streams through the narrow [window], illuminating dust motes dancing in the air. The loft is quiet and peaceful, with soft straw mattresses inviting rest after a long night.")
  
  (add-time-description 'tavern-loft :afternoon
                        "The loft is warm and comfortable in the afternoon light. A narrow [window] offers a view of the bustling village below, while the straw mattresses provide a quiet retreat from the day's activities.")
  
  (add-time-description 'tavern-loft :evening
                        "As evening approaches, the loft grows dim and cozy. A narrow [window] frames the setting sun, casting long shadows across the room. The straw mattresses look inviting for weary travelers.")
  
  (add-time-description 'tavern-loft :dusk
                        "The loft grows intimate as dusk falls, with the narrow [window] showing the village lights beginning to twinkle. The straw mattresses are ready for those seeking rest after a day's adventures.")
  
  (add-time-description 'tavern-loft :night
                        "The loft is quiet and peaceful in the darkness, with only the narrow [window] revealing the silver glow of the moonlit treeline beyond the village walls. The straw mattresses offer comfort for the night.")

  ;; Moonlit Lane time descriptions
  (add-time-description 'moonlit-lane :dawn
                        "The lane is quiet in the early morning, with [ivy-clad cottages] still dark and sleeping. The first light of dawn begins to illuminate the [forest archway] ahead, while the dirt road to the north waits for travelers.")
  
  (add-time-description 'moonlit-lane :morning
                        "The lane comes alive with morning activity, [ivy-clad cottages] showing signs of life as villagers begin their day. The [forest archway] catches the morning light, and the dirt road to the north beckons to adventurers.")
  
  (add-time-description 'moonlit-lane :afternoon
                        "The lane is warm and pleasant in the afternoon sun, with [ivy-clad cottages] basking in the light. The [forest archway] provides welcome shade, while the dirt road to the north is busy with travelers.")
  
  (add-time-description 'moonlit-lane :evening
                        "As evening approaches, the lane grows more mysterious, with [ivy-clad cottages] casting long shadows. The [forest archway] seems to beckon with ancient secrets, while the dirt road to the north fades into the distance.")
  
  (add-time-description 'moonlit-lane :dusk
                        "The lane becomes atmospheric as dusk falls, with [ivy-clad cottages] silhouetted against the darkening sky. The [forest archway] grows more foreboding, and the dirt road to the north disappears into the gathering darkness.")
  
  (add-time-description 'moonlit-lane :night
                        "A narrow lane stretches eastward, flanked by [ivy-clad cottages]. Fireflies dance in the night air, drawing the eye toward the shadowed [forest archway]. A dirt road branches off to the north, mysterious in the moonlight.")

  ;; Ancient Grove time descriptions
  (add-time-description 'ancient-grove :dawn
                        "The grove is shrouded in morning mist, with [standing stones] emerging like ancient sentinels from the fog. The air hums with primal power as the first light touches the deep claw marks on the surrounding trees.")
  
  (add-time-description 'ancient-grove :morning
                        "Morning light filters into the hidden grove, illuminating the ring of [standing stones] with golden rays. The air hums with primal power, and the deep claw marks on the surrounding trees are clearly visible in the daylight.")
  
  (add-time-description 'ancient-grove :afternoon
                        "The grove is warm and alive in the afternoon sun, with [standing stones] casting intricate shadows. The air hums with primal power, and the deep claw marks on the surrounding trees tell tales of the Forest Guardian's presence.")
  
  (add-time-description 'ancient-grove :evening
                        "As evening approaches, the grove grows more mystical, with [standing stones] catching the last golden rays. The air hums with primal power, and the deep claw marks on the surrounding trees seem to glow with ancient energy.")
  
  (add-time-description 'ancient-grove :dusk
                        "The grove becomes mysterious as dusk falls, with [standing stones] silhouetted against the darkening sky. The air hums with primal power, and the deep claw marks on the surrounding trees are barely visible in the fading light.")
  
  (add-time-description 'ancient-grove :night
                        "Moonlight spills into this hidden grove, illuminating a ring of [standing stones]. The air hums with primal power, and deep claw marks score the surrounding trees. A hush falls here -- the domain of the Forest Guardian.")

  ;; Market Stalls time descriptions
  (add-time-description 'market-stalls :dawn
                        "The market is quiet in the early morning, with canopies still closed and merchants preparing for the day. The lingering aroma of [roasted chestnuts] from yesterday mingles with the fresh morning air, while the [notice board] awaits new announcements.")
  
  (add-time-description 'market-stalls :morning
                        "The market comes alive with morning activity, canopies opening as merchants set up their stalls. The aroma of fresh [roasted chestnuts] fills the air, and the [notice board] displays the day's announcements for eager adventurers.")
  
  (add-time-description 'market-stalls :afternoon
                        "The market is bustling with midday activity, canopies providing welcome shade from the afternoon sun. The aroma of [roasted chestnuts] is strong, and the [notice board] is busy with merchants and travelers checking for opportunities.")
  
  (add-time-description 'market-stalls :evening
                        "As evening approaches, the market begins to wind down, with canopies starting to close. The aroma of [roasted chestnuts] grows stronger as merchants prepare to leave, while the [notice board] shows the day's final announcements.")
  
  (add-time-description 'market-stalls :dusk
                        "The market grows quiet as dusk falls, with most canopies closed and merchants packing their wares. The lingering aroma of [roasted chestnuts] and fresh parchment fills the air, while the [notice board] displays various announcements in the fading light.")
  
  (add-time-description 'market-stalls :night
                        "Canopies ripple in the breeze as merchants shutter their stalls. The lingering aroma of [roasted chestnuts] and fresh parchment fills the air. A [notice board] displays various announcements, mysterious in the moonlight.")

  ;; Market Stalls time-based names
  (add-time-name 'market-stalls :dawn "Dawn Market")
  (add-time-name 'market-stalls :morning "Opening Market")
  (add-time-name 'market-stalls :afternoon "Busy Market")
  (add-time-name 'market-stalls :evening "Closing Market")
  (add-time-name 'market-stalls :dusk "Evening Market")
  (add-time-name 'market-stalls :night "Closed Market")

  ;; Riverbank time descriptions
  (add-time-description 'riverbank :dawn
                        "The riverbank is peaceful in the early morning, with the [river] reflecting the first light of dawn. A wooden skiff knocks gently against the pier, ready for anyone bold enough to cast off into the misty waters.")
  
  (add-time-description 'riverbank :morning
                        "Morning light paints the [river] in silver ribbons, creating a beautiful scene. A wooden skiff knocks gently against the pier, ready for anyone bold enough to cast off into the clear morning waters.")
  
  (add-time-description 'riverbank :afternoon
                        "The riverbank is warm and inviting in the afternoon sun, with the [river] sparkling in the light. A wooden skiff knocks gently against the pier, ready for anyone bold enough to cast off into the peaceful waters.")
  
  (add-time-description 'riverbank :evening
                        "As evening approaches, the riverbank becomes more atmospheric, with the [river] reflecting the golden sunset. A wooden skiff knocks gently against the pier, ready for anyone bold enough to cast off into the mysterious evening waters.")
  
  (add-time-description 'riverbank :dusk
                        "The riverbank grows mysterious as dusk falls, with the [river] becoming dark and foreboding. A wooden skiff knocks gently against the pier, ready for anyone bold enough to cast off into the shadowy waters.")
  
  (add-time-description 'riverbank :night
                        "Moonlight paints the [river] in silver ribbons. A wooden skiff knocks gently against the pier, ready for anyone bold enough to cast off into the mysterious night waters.")

  ;; Hidden Cove time descriptions
  (add-time-description 'hidden-cove :dawn
                        "The cove is shrouded in morning mist, with ancient [ruins] barely visible through the vines on the cliff face. The water is calm and crystal clear, while a narrow [cave entrance] yawns darkly in the rock. A weathered [shipwreck] lies half-submerged in the misty waters.")
  
  (add-time-description 'hidden-cove :morning
                        "Morning light illuminates the secluded cove, revealing ancient [ruins] peeking through the vines on the cliff face. The water is calm and crystal clear, while a narrow [cave entrance] yawns in the rock. A weathered [shipwreck] lies half-submerged near the shore.")
  
  (add-time-description 'hidden-cove :afternoon
                        "The cove is warm and inviting in the afternoon sun, with ancient [ruins] clearly visible through the vines on the cliff face. The water is calm and crystal clear, while a narrow [cave entrance] yawns in the rock. A weathered [shipwreck] lies half-submerged, its details visible in the bright light.")
  
  (add-time-description 'hidden-cove :evening
                        "As evening approaches, the cove becomes more mysterious, with ancient [ruins] casting long shadows through the vines on the cliff face. The water is calm and crystal clear, while a narrow [cave entrance] yawns darkly in the rock. A weathered [shipwreck] lies half-submerged, its secrets hidden in the fading light.")
  
  (add-time-description 'hidden-cove :dusk
                        "The cove grows atmospheric as dusk falls, with ancient [ruins] becoming dark silhouettes against the cliff face. The water is calm and crystal clear, while a narrow [cave entrance] yawns ominously in the rock. A weathered [shipwreck] lies half-submerged, mysterious in the gathering darkness.")
  
  (add-time-description 'hidden-cove :night
                        "A secluded cove surrounded by towering cliffs. The water is calm here, crystal clear. Ancient [ruins] peek through the vines on the cliff face, and a narrow [cave entrance] yawns in the rock. A weathered [shipwreck] lies half-submerged near the shore, mysterious in the moonlight.")

  ;; Village Garden time descriptions
  (add-time-description 'village-garden :dawn
                        "The garden is peaceful in the early morning, with [herbs] glistening with dew and the [apple tree] standing silent in the mist. The village elder's careful tending is evident in the neat rows of vegetables.")
  
  (add-time-description 'village-garden :morning
                        "Morning light brings the garden to life, with [herbs] releasing their sweet scent and the [apple tree] catching the first rays. The village elder's careful tending is evident in the thriving rows of vegetables.")
  
  (add-time-description 'village-garden :afternoon
                        "The garden is warm and productive in the afternoon sun, with [herbs] basking in the light and the [apple tree] providing welcome shade. The village elder's careful tending is evident in the flourishing rows of vegetables.")
  
  (add-time-description 'village-garden :evening
                        "As evening approaches, the garden grows peaceful, with [herbs] releasing their evening fragrance and the [apple tree] casting long shadows. The village elder's careful tending is evident in the well-maintained rows of vegetables.")
  
  (add-time-description 'village-garden :dusk
                        "The garden becomes atmospheric as dusk falls, with [herbs] silhouetted against the darkening sky and the [apple tree] standing like a guardian. The village elder's careful tending is evident in the neat rows of vegetables.")
  
  (add-time-description 'village-garden :night
                        "A small, peaceful garden bursting with life. Rows of vegetables grow alongside fragrant [herbs], and a magnificent [apple tree] stands in the center, its branches heavy with ripe red fruit. The village elder tends to this garden with great care, even in the moonlight.")

  ;; Sky rooms time descriptions (simplified since they're aerial views)
  (add-time-description 'sky-over-village :dawn
                        "You soar high above the village in the early morning light, the UFO humming beneath you. Below, the [village square] is a small cobblestone circle, the [tavern] shows signs of morning activity, and the [market] is just beginning to open. To the east, dark [woods] stretch like a shadowy blanket. To the northwest, the [graveyard] glows with ethereal light.")
  
  (add-time-description 'sky-over-village :morning
                        "You soar high above the village in the morning light, the UFO humming beneath you. Below, the [village square] is bustling with activity, the [tavern] shows signs of life, and the [market] is alive with merchants. To the east, dark [woods] stretch like a shadowy blanket. To the northwest, the [graveyard] glows with ethereal light.")
  
  (add-time-description 'sky-over-village :afternoon
                        "You soar high above the village in the afternoon sun, the UFO humming beneath you. Below, the [village square] is busy with midday activity, the [tavern] shows signs of life, and the [market] is bustling with merchants. To the east, dark [woods] stretch like a shadowy blanket. To the northwest, the [graveyard] glows with ethereal light.")
  
  (add-time-description 'sky-over-village :evening
                        "You soar high above the village as evening approaches, the UFO humming beneath you. Below, the [village square] is winding down, the [tavern] shows signs of evening activity, and the [market] is closing for the day. To the east, dark [woods] stretch like a shadowy blanket. To the northwest, the [graveyard] glows with ethereal light.")
  
  (add-time-description 'sky-over-village :dusk
                        "You soar high above the village as dusk falls, the UFO humming beneath you. Below, the [village square] is quiet, the [tavern] shows warm lights, and the [market] is closed for the night. To the east, dark [woods] stretch like a shadowy blanket. To the northwest, the [graveyard] glows with ethereal light.")
  
  (add-time-description 'sky-over-village :night
                        "You soar high above the village, the UFO humming beneath you. Below, the [village square] is a small cobblestone circle, the [tavern] a warm glow, and the [market] a cluster of colorful canopies. To the east, dark [woods] stretch like a shadowy blanket. To the northwest, the [graveyard] glows with ethereal light.")

  ;; Highway rooms time descriptions
  (add-time-description 'highway-north :dawn
                        "A wide dirt road stretches north and south in the early morning light, flanked by rolling hills and scattered trees. The road is well-maintained and perfect for vehicles, with dust clouds rising from the occasional early traveler.")
  
  (add-time-description 'highway-north :morning
                        "A wide dirt road stretches north and south in the morning light, flanked by rolling hills and scattered trees. The road is well-maintained and perfect for vehicles, with dust clouds rising from the morning traffic.")
  
  (add-time-description 'highway-north :afternoon
                        "A wide dirt road stretches north and south in the afternoon sun, flanked by rolling hills and scattered trees. The road is well-maintained and perfect for vehicles, with dust clouds rising from the busy afternoon traffic.")
  
  (add-time-description 'highway-north :evening
                        "A wide dirt road stretches north and south as evening approaches, flanked by rolling hills and scattered trees. The road is well-maintained and perfect for vehicles, with dust clouds rising from the evening travelers.")
  
  (add-time-description 'highway-north :dusk
                        "A wide dirt road stretches north and south as dusk falls, flanked by rolling hills and scattered trees. The road is well-maintained and perfect for vehicles, with dust clouds rising from the occasional passing traveler in the fading light.")
  
  (add-time-description 'highway-north :night
                        "A wide dirt road stretches north and south, flanked by rolling hills and scattered trees. The road is well-maintained and perfect for vehicles. Dust clouds rise from the occasional passing traveler, mysterious in the moonlight.")

  (add-time-description 'highway-crossroads :dawn
                        "A major intersection where several roads meet in the early morning light. A weathered signpost points in multiple directions, and the ground is worn smooth by countless wheels. This is clearly a well-traveled route for vehicles.")
  
  (add-time-description 'highway-crossroads :morning
                        "A major intersection where several roads meet in the morning light. A weathered signpost points in multiple directions, and the ground is worn smooth by countless wheels. This is clearly a well-traveled route for vehicles.")
  
  (add-time-description 'highway-crossroads :afternoon
                        "A major intersection where several roads meet in the afternoon sun. A weathered signpost points in multiple directions, and the ground is worn smooth by countless wheels. This is clearly a well-traveled route for vehicles.")
  
  (add-time-description 'highway-crossroads :evening
                        "A major intersection where several roads meet as evening approaches. A weathered signpost points in multiple directions, and the ground is worn smooth by countless wheels. This is clearly a well-traveled route for vehicles.")
  
  (add-time-description 'highway-crossroads :dusk
                        "A major intersection where several roads meet as dusk falls. A weathered signpost points in multiple directions, and the ground is worn smooth by countless wheels. This is clearly a well-traveled route for vehicles.")
  
  (add-time-description 'highway-crossroads :night
                        "A major intersection where several roads meet. A weathered signpost points in multiple directions, and the ground is worn smooth by countless wheels. This is clearly a well-traveled route for vehicles, mysterious in the moonlight.")

  ;; Additional sky rooms time descriptions
  (add-time-description 'sky-over-forest :dawn
                        "The UFO glides silently over the vast expanse of the [Whispering Wood] in the early morning light. The pine canopy below ripples like dark green waves. You can see a [clearing] and what looks like a [standing stone] poking through the trees. To the west, the village lights twinkle.")
  
  (add-time-description 'sky-over-forest :morning
                        "The UFO glides silently over the vast expanse of the [Whispering Wood] in the morning light. The pine canopy below ripples like dark green waves. You can see a [clearing] and what looks like a [standing stone] poking through the trees. To the west, the village lights twinkle.")
  
  (add-time-description 'sky-over-forest :afternoon
                        "The UFO glides silently over the vast expanse of the [Whispering Wood] in the afternoon sun. The pine canopy below ripples like dark green waves. You can see a [clearing] and what looks like a [standing stone] poking through the trees. To the west, the village lights twinkle.")
  
  (add-time-description 'sky-over-forest :evening
                        "The UFO glides silently over the vast expanse of the [Whispering Wood] as evening approaches. The pine canopy below ripples like dark green waves. You can see a [clearing] and what looks like a [standing stone] poking through the trees. To the west, the village lights twinkle.")
  
  (add-time-description 'sky-over-forest :dusk
                        "The UFO glides silently over the vast expanse of the [Whispering Wood] as dusk falls. The pine canopy below ripples like dark green waves. You can see a [clearing] and what looks like a [standing stone] poking through the trees. To the west, the village lights twinkle.")
  
  (add-time-description 'sky-over-forest :night
                        "The UFO glides silently over the vast expanse of the [Whispering Wood]. The pine canopy below ripples like dark green waves. You can see a [clearing] and what looks like a [standing stone] poking through the trees. To the west, the village lights twinkle.")

  (add-time-description 'sky-over-market :dawn
                        "You hover above the [market district] in the early morning light, watching the first merchants begin to set up their stalls. The [riverbank] glitters to the south where morning light dances on water. The village square lies to the east, and beyond it, the world awakens.")
  
  (add-time-description 'sky-over-market :morning
                        "You hover above the [market district] in the morning light, watching merchants set up their stalls. The [riverbank] glitters to the south where morning light dances on water. The village square lies to the east, and beyond it, the world comes alive.")
  
  (add-time-description 'sky-over-market :afternoon
                        "You hover above the [market district] in the afternoon sun, watching the bustling market activity below. The [riverbank] glitters to the south where afternoon light dances on water. The village square lies to the east, and beyond it, the world is busy.")
  
  (add-time-description 'sky-over-market :evening
                        "You hover above the [market district] as evening approaches, watching the last merchants pack their wares. The [riverbank] glitters to the south where evening light dances on water. The village square lies to the east, and beyond it, the world winds down.")
  
  (add-time-description 'sky-over-market :dusk
                        "You hover above the [market district] as dusk falls, watching the last merchants pack their wares. The [riverbank] glitters to the south where dusk light dances on water. The village square lies to the east, and beyond it, darkness.")
  
  (add-time-description 'sky-over-market :night
                        "You hover above the [market district], watching the last merchants pack their wares. The [riverbank] glitters to the south where moonlight dances on water. The village square lies to the east, and beyond it, darkness.")

  (add-time-description 'sky-over-river :dawn
                        "The UFO drifts above the winding [river] in the early morning light, its waters shimmering below. You can trace its path south to a [hidden cove] surrounded by sheer cliffs. To the north, the market district is visible in the morning light.")
  
  (add-time-description 'sky-over-river :morning
                        "The UFO drifts above the winding [river] in the morning light, its waters shimmering below. You can trace its path south to a [hidden cove] surrounded by sheer cliffs. To the north, the market district is visible in the morning light.")
  
  (add-time-description 'sky-over-river :afternoon
                        "The UFO drifts above the winding [river] in the afternoon sun, its waters shimmering below. You can trace its path south to a [hidden cove] surrounded by sheer cliffs. To the north, the market district is visible in the afternoon light.")
  
  (add-time-description 'sky-over-river :evening
                        "The UFO drifts above the winding [river] as evening approaches, its waters shimmering below. You can trace its path south to a [hidden cove] surrounded by sheer cliffs. To the north, the market district is visible in the evening light.")
  
  (add-time-description 'sky-over-river :dusk
                        "The UFO drifts above the winding [river] as dusk falls, its waters shimmering below. You can trace its path south to a [hidden cove] surrounded by sheer cliffs. To the north, the market district is visible in the fading light.")
  
  (add-time-description 'sky-over-river :night
                        "The UFO drifts above the winding [river], its waters shimmering below. You can trace its path south to a [hidden cove] surrounded by sheer cliffs. To the north, the market district is visible in the moonlight.")

  (add-time-description 'sky-over-graveyard :dawn
                        "The UFO hovers silently above the ancient [graveyard] in the early morning light. From this height, the tombstones look like scattered bones, and the [ethereal mist] glows eerily in the morning light. To the southeast, the village square is visible.")
  
  (add-time-description 'sky-over-graveyard :morning
                        "The UFO hovers silently above the ancient [graveyard] in the morning light. From this height, the tombstones look like scattered bones, and the [ethereal mist] glows eerily in the morning light. To the southeast, the village square is visible.")
  
  (add-time-description 'sky-over-graveyard :afternoon
                        "The UFO hovers silently above the ancient [graveyard] in the afternoon sun. From this height, the tombstones look like scattered bones, and the [ethereal mist] glows eerily in the afternoon light. To the southeast, the village square is visible.")
  
  (add-time-description 'sky-over-graveyard :evening
                        "The UFO hovers silently above the ancient [graveyard] as evening approaches. From this height, the tombstones look like scattered bones, and the [ethereal mist] glows eerily in the evening light. To the southeast, the village square is visible.")
  
  (add-time-description 'sky-over-graveyard :dusk
                        "The UFO hovers silently above the ancient [graveyard] as dusk falls. From this height, the tombstones look like scattered bones, and the [ethereal mist] glows eerily in the dusk light. To the southeast, the village square is visible.")
  
  (add-time-description 'sky-over-graveyard :night
                        "The UFO hovers silently above the ancient [graveyard]. From this height, the tombstones look like scattered bones, and the [ethereal mist] glows eerily in the moonlight. To the southeast, the village square is visible."))

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
                ;; Ground exits - accessible to all vehicles and on foot
                ((eq exit-type :ground)
                 (cdr rest-of-entry))
                ;; Other typed exits - vehicle must match
                (t
                 (when (and vehicle-type (eq vehicle-type exit-type))
                   (cdr rest-of-entry)))))
            ;; Simple exit (format: (:direction . room))
            ;; Accessible when not in a vehicle OR in an uber/air/ground vehicle
            (when (or (null vehicle-type) (eq vehicle-type :uber) (eq vehicle-type :air) (eq vehicle-type :ground))
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

