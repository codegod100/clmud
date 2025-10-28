(in-package :mud.world)

(defun debug-map-render (current-room-id &optional vehicle-type)
  "Debug version of map render with output."
  (format t "DEBUG: Starting map render for room ~a~%" current-room-id)
  (let ((info (make-hash-table :test #'eq))
        (queue (list (list current-room-id 0 0 0))))
    (setf (gethash current-room-id info) (list 0 0 0))
    (format t "DEBUG: Initial queue: ~a~%" queue)
    (format t "DEBUG: Initial info: ~a~%" info)
    (loop while queue do
      (destructuring-bind (room-id depth x y) (pop queue)
        (format t "DEBUG: Processing room ~a at depth ~a, position (~a, ~a)~%" room-id depth x y)
        (when (< depth 2)
          (let ((room (find-room room-id)))
            (format t "DEBUG: Found room: ~a~%" room)
            (when room
              (let ((exits (room-exits room)))
                (format t "DEBUG: Room exits: ~a~%" exits)
                (dolist (exit exits)
                  (let* ((direction (car exit))
                         (delta (direction-delta direction)))
                    (destructuring-bind (dx dy) delta
                      (unless (and (= dx 0) (= dy 0))
                        (let ((dest (neighbor room direction vehicle-type)))
                          (format t "DEBUG: Direction ~a -> ~a~%" direction dest)
                          (when (and dest (not (gethash dest info)))
                            (let ((next-depth (1+ depth))
                                  (nx (+ x dx))
                                  (ny (+ y dy)))
                              (format t "DEBUG: Adding room ~a at depth ~a, position (~a, ~a)~%" dest next-depth nx ny)
                              (setf (gethash dest info) (list next-depth nx ny))
                              (push (list dest next-depth nx ny) queue))))))))))))
    (format t "DEBUG: Final info: ~a~%" info)
    (if (zerop (hash-table-count info))
        "The world is shrouded in mystery..."
        "Map would be rendered here...")))

(defun generate-artistic-map (current-room-id &optional vehicle-type)
  "Generate the adjacency-based ASCII map used by the MAP command."
  (debug-map-render current-room-id vehicle-type))
))
