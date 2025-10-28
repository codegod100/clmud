(in-package :mud.world)

(defun build-adjacency-map (current-room-id &optional vehicle-type)
  "Return hash of room-id -> (depth x y) for rooms within two steps of CURRENT-ROOM-ID."
  (let ((info (make-hash-table :test #'eq)))
    (let ((queue (list (list current-room-id 0 0 0))))
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
                              (push (list dest next-depth nx ny) queue)))))))))))
    info))

(defun map-truncate-name (string width)
  "Truncate string to fit within width, adding ... if needed."
  (if (> (length string) width)
      (concatenate 'string (subseq string 0 (- width 3)) "...")
      string))

(defun map-room-label (room-id width)
  "Get room label, truncated to fit width."
  (let* ((room (find-room room-id))
         (name (if room
                   (room-name room)
                   (string-capitalize (symbol-name room-id)))))
    (map-truncate-name name width)))

(defun map-cell-string (room-id depth width)
  "Format cell content with * for current room."
  (if (zerop depth)
      (let ((base (map-room-label room-id (1- width))))
        (map-truncate-name (concatenate 'string base "*") width))
      (map-room-label room-id width)))

(defun map-pad-string (string width)
  "Pad string to exact width with spaces."
  (let* ((text (or string ""))
         (len (length text)))
    (if (< len width)
        (concatenate 'string text (make-string (- width len) :initial-element #\Space))
        text)))

(defun generate-adjacency-map (current-room-id &optional vehicle-type)
  "Render a 2-level adjacency grid centred on CURRENT-ROOM-ID."
  (let* ((info (build-adjacency-map current-room-id vehicle-type))
         (cells '()))
    (maphash (lambda (room-id data)
               (destructuring-bind (depth x y) data
                 (push (list room-id depth x y) cells)))
             info)
    (if (null cells)
        "The world is shrouded in mystery..."
        (let ((min-x 0)
              (max-x 0)
              (min-y 0)
              (max-y 0))
          (dolist (cell cells)
            (destructuring-bind (room-id depth x y) cell
              (declare (ignore room-id depth))
              (setf min-x (min min-x x))
              (setf max-x (max max-x x))
              (setf min-y (min min-y y))
              (setf max-y (max max-y y))))
          (let* ((width (+ (- max-x min-x) 1))
                 (height (+ (- max-y min-y) 1))
                 (grid (make-array (list height width) :initial-element nil))
                 (cell-width 13))
            (dolist (cell cells)
              (destructuring-bind (room-id depth x y) cell
                (let ((row (- y min-y))
                      (col (- x min-x)))
                  (setf (aref grid row col) (list room-id depth)))))
            (with-output-to-string (s)
              (format s "~%")
              (format s "    +======================================================================+~%")
              (format s "    |  Orientation: North at top, South bottom, East right, West left.    |~%")
              (format s "    |                                                              |~%")
              (loop for row from 0 below height do
                (format s "    |")
                (loop for col from 0 below width do
                  (let ((cell (aref grid row col)))
                    (format s " ~a"
                            (map-pad-string (if cell
                                                (destructuring-bind (room-id depth) cell
                                                  (map-cell-string room-id depth cell-width))
                                                "")
                                            cell-width))))
                (format s " |~%"))
              (format s "    |                                                              |~%")
              (format s "    |  Legend: * = current location, depth 1 = adjacent, depth 2 = nearby  |~%")
              (format s "    +======================================================================+~%")
              (format s "~%"))))))

(defun generate-artistic-map (current-room-id &optional vehicle-type)
  "Generate the adjacency-based ASCII map used by the MAP command."
  (generate-adjacency-map current-room-id vehicle-type))
))
))
