(in-package :mud.world)

(defstruct room
  id
  name
  description
  exits)

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
               '((:north . market-stalls))))

(defun find-room (room-id)
  (gethash room-id *rooms*))

(defun starting-room ()
  (or (find-room *starting-room*)
      (error "Starting room ~a is undefined." *starting-room*)))

(defun neighbor (room direction)
  (cdr (assoc direction (room-exits room))))
