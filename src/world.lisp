(in-package :mud.world)

(defstruct room
  id
  name
  description
  exits
  (items nil :type list))

(defparameter *rooms* (make-hash-table :test #'eq))
(defparameter *starting-room* 'village-square)

(defun define-room (id name description exits)
  (setf (gethash id *rooms*)
        (make-room :id id :name name :description description :exits exits)))

(defun initialize-world ()
  (clrhash *rooms*)
  (define-room 'village-square
               "Village Square"
               "Cobblestone paths converge beneath an ancient oak, its leaves whispering tales of heroes past. Lanterns sway gently, casting golden halos in the dusk."
               '((:north . tavern-common-room)
                 (:east . moonlit-lane)
                 (:west . market-stalls)))
  (define-room 'tavern-common-room
               "The Bronze Badger"
               "Warm lamplight spills over polished oak tables, while the scent of spiced cider mingles with distant lute music. A crackling hearth invites weary travelers."
               '((:south . village-square)
                 (:up . tavern-loft)))
  (define-room 'tavern-loft
               "Tavern Loft"
               "Low beams and soft straw mattresses offer respite. A narrow window reveals the silver glow of the moonlit treeline beyond the village walls."
               '((:down . tavern-common-room)))
  (define-room 'moonlit-lane
               "Moonlit Lane"
               "A narrow lane stretches eastward, flanked by ivy-clad cottages. Fireflies dance in the night air, drawing the eye toward the shadowed forest archway."
               '((:west . village-square)
                 (:east . whispering-wood)))
  (define-room 'whispering-wood
               "Whispering Wood"
               "Towering pines murmur secrets overhead, their needles shimmering with dew. Somewhere deeper within, an owl hoots, beckoning the brave."
               '((:west . moonlit-lane)))
  (define-room 'market-stalls
               "Closing Market"
               "Canopies ripple in the breeze as merchants shutter their stalls. The lingering aroma of roasted chestnuts and fresh parchment fills the air."
               '((:east . village-square)
                 (:south . riverbank)))
  (define-room 'riverbank
               "Riverbank"
               "Moonlight paints the river in silver ribbons. A wooden skiff knocks gently against the pier, ready for anyone bold enough to cast off."
               '((:north . market-stalls)))
  (define-room 'graveyard
               "Graveyard"
               "Ancient tombstones lean in the mist, their inscriptions worn by time. The air is still, heavy with the weight of countless souls who have passed through this veil. A faint ethereal glow marks the boundary between life and death."
               '((:south . village-square))))

(defun find-room (room-id)
  (gethash room-id *rooms*))

(defun starting-room ()
  (or (find-room *starting-room*)
      (error "Starting room ~a is undefined." *starting-room*)))

(defun neighbor (room direction)
  (cdr (assoc direction (room-exits room))))

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
    (format s "                    [Graveyard]~a~%"
            (if (eq current-room-id 'graveyard) " *" ""))
    (format s "                         |~%")
    (format s "                         |~%")
    (format s "                  [Tavern Loft]~a~%"
            (if (eq current-room-id 'tavern-loft) " *" ""))
    (format s "                         |~%")
    (format s "                  [Bronze Badger]~a~%"
            (if (eq current-room-id 'tavern-common-room) " *" ""))
    (format s "                         |~%")
    (format s "                         |~%")
    (format s "  [Market]-----[Village Square]-----[Moonlit Lane]-----[Whispering Wood]~%")
    (format s "  ~a           ~a              ~a                ~a~%"
            (if (eq current-room-id 'market-stalls) "*" " ")
            (if (eq current-room-id 'village-square) "*" " ")
            (if (eq current-room-id 'moonlit-lane) "*" " ")
            (if (eq current-room-id 'whispering-wood) "*" " "))
    (format s "     |~%")
    (format s "     |~%")
    (format s "  [Riverbank]~a~%"
            (if (eq current-room-id 'riverbank) " *" ""))
    (format s "~%")
    (format s "  * = Your current location~%")))
