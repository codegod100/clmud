(in-package :mud.world)

(defun debug-map-simple (current-room-id &optional vehicle-type)
  "Simple debug map that shows what's happening."
  (format t "DEBUG: Map called with room ~a~%" current-room-id)
  (let ((room (find-room current-room-id)))
    (format t "DEBUG: Found room ~a~%" room)
    (if room
        (progn
          (format t "DEBUG: Room name: ~a~%" (room-name room))
          (format t "DEBUG: Room exits: ~a~%" (room-exits room))
          "DEBUG: Map would be rendered here...")
        "DEBUG: No room found")))

(defun generate-artistic-map (current-room-id &optional vehicle-type)
  "Generate the adjacency-based ASCII map used by the MAP command."
  (debug-map-simple current-room-id vehicle-type))
