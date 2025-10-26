(load "~/quicklisp/setup.lisp")

;; Load dependencies in the same order as the main entry point, but without
;; starting the server.
(require :sb-bsd-sockets)
(require :asdf)
(load "src/packages.lisp")
(load "src/ansi.lisp")
(load "src/player.lisp")
(load "src/inventory.lisp")
(load "src/world.lisp")
(load "src/mob.lisp")
(load "src/combat.lisp")
(load "src/quest.lisp")
(load "src/server/core.lisp")
(load "src/server/commands.lisp")
(load "src/server/runtime.lisp")

;; Initialize the world
(mud.world:initialize-world)

(format t "~&~%========================================~%")
(format t "MUD REPL - Debug Environment Loaded~%")
(format t "========================================~%~%")
(format t "Available packages:~%")
(format t "  - mud.world~%")
(format t "  - mud.player~%")
(format t "  - mud.mob~%")
(format t "  - mud.combat~%")
(format t "  - mud.quest~%")
(format t "  - mud.server~%~%")

(format t "Useful commands:~%")
(format t "  (show-rooms)        - List all defined rooms~%")
(format t "  (show-sky-rooms)    - List sky rooms and exits~%")
(format t "  (show-room 'id)     - Inspect specific room~%")
(format t "  (make-test-player)  - Create a test player~%~%")

;; Helper functions for debugging
(defun show-rooms ()
  "List all defined rooms."
  (format t "~&Defined rooms:~%")
  (maphash (lambda (id room)
             (format t "  ~A: ~A~%" id (mud.world::room-name room)))
           mud.world::*rooms*))

(defun show-sky-rooms ()
  "List sky rooms and their exits."
  (format t "~&Sky rooms:~%")
  (maphash (lambda (id room)
             (when (search "SKY" (string id))
               (format t "  ~A: ~A~%" id (mud.world::room-name room))
               (format t "    Exits: ~A~%" (mud.world::room-exits room))))
           mud.world::*rooms*))

(defun show-room (room-id)
  "Inspect a specific room."
  (let ((room (gethash room-id mud.world::*rooms*)))
    (if room
        (progn
          (format t "~&Room: ~A~%" room-id)
          (format t "Name: ~A~%" (mud.world::room-name room))
          (format t "Description: ~A~%" (mud.world::room-description room))
          (format t "Exits: ~A~%" (mud.world::room-exits room))
          (format t "Items: ~A~%" (mud.world::room-items room)))
        (format t "~&Room ~A not found.~%" room-id))))

(defun make-test-player (&key (name "Tester") (room 'mud.world::sky-platform-central))
  "Create a test player for debugging."
  (let ((player (mud.player:make-player :name name
                                        :room room
                                        :stream *standard-output*
                                        :socket nil)))
    (mud.player:set-player-room player room)
    (format t "~&Created player ~A in room ~A~%" name room)
    player))

(defun get-exit-room (exit)
  "Extract room-id from exit, handling both simple and complex formats."
  (cond
    ((symbolp exit) exit)                      ; Simple: ROOM-ID
    ((consp exit)                              ; Complex: (DIR requirement . ROOM-ID)
     (if (consp (cdr exit))
         (cddr exit)                           ; (DIR REQ . ROOM-ID)
         (cdr exit)))                          ; (DIR . ROOM-ID)
    (t nil)))

(defun get-exit-direction (exit-entry)
  "Get direction keyword from exit entry."
  (car exit-entry))

(defun visualize-sky-map (&optional (start-room 'mud.world::sky-over-village))
  "Show a simple text visualization of connected sky rooms."
  (let ((visited (make-hash-table))
        (room-data (make-hash-table)))
    
    ;; Collect all reachable rooms
    (labels ((collect-rooms (room-id depth)
               (when (and room-id 
                         (not (gethash room-id visited))
                         (<= depth 3))
                 (setf (gethash room-id visited) t)
                 (let ((room (gethash room-id mud.world::*rooms*)))
                   (when room
                     (setf (gethash room-id room-data) room)
                     (dolist (exit-entry (mud.world::room-exits room))
                       (let ((next-room (get-exit-room exit-entry)))
                         (collect-rooms next-room (1+ depth)))))))))
      (collect-rooms start-room 0))
    
    ;; Display rooms and connections
    (format t "~&~%Sky Map (starting from ~A):~%" start-room)
    (format t "~%")
    (maphash (lambda (room-id room)
               (format t "~A~%" room-id)
               (format t "  └─ ~A~%" (mud.world::room-name room))
               (dolist (exit-entry (mud.world::room-exits room))
                 (let ((dir (get-exit-direction exit-entry))
                       (dest (get-exit-room exit-entry)))
                   (format t "     ~A -> ~A~%" dir dest)))
               (format t "~%"))
             room-data)))

(defun draw-sky-map (&optional (center-room 'mud.world::sky-over-village) (radius 2))
  "Draw an ASCII map of sky rooms centered on a room."
  (let ((positions (make-hash-table))
        (visited (make-hash-table))
        (min-x 0) (max-x 0) (min-y 0) (max-y 0))
    
    ;; Assign coordinates based on cardinal directions
    (labels ((assign-position (room-id x y depth)
               (when (and room-id
                         (not (gethash room-id visited))
                         (<= depth radius))
                 (setf (gethash room-id visited) t)
                 (setf (gethash room-id positions) (cons x y))
                 (setf min-x (min min-x x) max-x (max max-x x))
                 (setf min-y (min min-y y) max-y (max max-y y))
                 
                 (let ((room (gethash room-id mud.world::*rooms*)))
                   (when room
                     (dolist (exit-entry (mud.world::room-exits room))
                       (let ((dir (get-exit-direction exit-entry))
                             (dest (get-exit-room exit-entry)))
                         (case dir
                           (:north     (assign-position dest x (1- y) (1+ depth)))
                           (:south     (assign-position dest x (1+ y) (1+ depth)))
                           (:east      (assign-position dest (1+ x) y (1+ depth)))
                           (:west      (assign-position dest (1- x) y (1+ depth)))
                           (:northeast (assign-position dest (1+ x) (1- y) (1+ depth)))
                           (:northwest (assign-position dest (1- x) (1- y) (1+ depth)))
                           (:southeast (assign-position dest (1+ x) (1+ y) (1+ depth)))
                           (:southwest (assign-position dest (1- x) (1+ y) (1+ depth)))))))))))
      
      (assign-position center-room 0 0 0))
    
    ;; Draw the map
    (format t "~&~%Sky Map (centered on ~A):~%~%" center-room)
    (loop for y from min-y to max-y do
      (loop for x from min-x to max-x do
        (let ((room-at-pos (loop for room-id being the hash-keys of positions
                                 using (hash-value pos)
                                 when (equal pos (cons x y))
                                 return room-id)))
          (if room-at-pos
              (format t "[~3A]" (subseq (string room-at-pos) 
                                       (max 0 (- (length (string room-at-pos)) 3))))
              (format t "     "))))
      (format t "~%"))
    
    ;; Legend
    (format t "~%Legend:~%")
    (maphash (lambda (room-id pos)
               (declare (ignore pos))
               (format t "  [~3A] = ~A~%" 
                       (subseq (string room-id) 
                               (max 0 (- (length (string room-id)) 3)))
                       (mud.world::room-name (gethash room-id mud.world::*rooms*))))
             positions)))

(format t "  (visualize-sky-map) - Show connected sky rooms~%")
(format t "  (draw-sky-map)      - Draw ASCII map of sky rooms~%~%")

(format t "Ready! Try (show-sky-rooms) to see the sky map.~%~%")