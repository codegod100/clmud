(in-package :mud.world)

(defun simple-adjacency-map (current-room-id &optional vehicle-type)
  "Simple 2-level adjacency map for testing."
  (let ((info (make-hash-table :test #'eq))
        (queue (list (list current-room-id 0 0 0))))
    (setf (gethash current-room-id info) (list 0 0 0))
    (loop while queue do
      (destructuring-bind (room-id depth x y) (pop queue)
        (when (< depth 2)
          (let ((room (find-room room-id)))
            (when room
              (dolist (exit (room-exits room))
                (let* ((direction (car exit))
                       (delta (direction-delta direction)))
                  (destructuring-bind (dx dy) delta
                    (unless (and (= dx 0) (= dy 0))
                      (let ((dest (neighbor room direction vehicle-type)))
                        (when (and dest (not (gethash dest info)))
                          (let ((next-depth (1+ depth))
                                (nx (+ x dx))
                                (ny (+ y dy)))
                            (setf (gethash dest info) (list next-depth nx ny))
                            (push (list dest next-depth nx ny) queue))))))))))))
    info))

(defun simple-map-render (current-room-id &optional vehicle-type)
  "Render simple 2-level adjacency map."
  (let* ((info (simple-adjacency-map current-room-id vehicle-type))
         (min-x 0) (max-x 0) (min-y 0) (max-y 0))
    (maphash (lambda (room-id data)
               (destructuring-bind (depth x y) data
                 (setf min-x (min min-x x))
                 (setf max-x (max max-x x))
                 (setf min-y (min min-y y))
                 (setf max-y (max max-y y))))
             info)
    (with-output-to-string (s)
      (format s "~%")
      (format s "    +======================================================================+~%")
      (format s "    |  Orientation: North at top, South bottom, East right, West left.    |~%")
      (format s "    |                                                              |~%")
      (loop for y from min-y to max-y do
        (format s "    |")
        (loop for x from min-x to max-x do
          (let ((data (gethash (list x y) info)))
            (if data
                (destructuring-bind (room-id depth) data
                  (let* ((room (find-room room-id))
                         (name (if room (room-name room) (symbol-name room-id)))
                         (label (if (zerop depth) (format nil "~a*" name) name)))
                    (format s " ~13a" (subseq (concatenate 'string label "            ") 0 13))))
                (format s " ~13a" ""))))
        (format s " |~%"))
      (format s "    |                                                              |~%")
      (format s "    |  Legend: * = current location, depth 1 = adjacent, depth 2 = nearby  |~%")
      (format s "    +======================================================================+~%")
      (format s "~%"))))
)
