(in-package :mud.server)

(defparameter *listener* nil)
(defparameter *listener-thread* nil)
(defparameter *clients* '())
(defparameter *clients-lock* (make-mutex :name "clients-lock"))
(defparameter *running* nil)

(defconstant +iac+ 255)
(defconstant +will+ 251)
(defconstant +wont+ 252)
(defconstant +do+ 253)
(defconstant +dont+ 254)
(defconstant +sb+ 250)
(defconstant +se+ 240)
(defconstant +ga+ 249)
(defconstant +cr+ 13)
(defconstant +lf+ 10)

(defun server-log (fmt &rest args)
  (apply #'format *error-output* (concatenate 'string fmt "~%") args)
  (finish-output *error-output*))

(defun make-server-socket (port)
  (let ((socket (make-instance 'inet-socket :type :stream :protocol :tcp)))
    (setf (sockopt-reuse-address socket) t)
    (socket-bind socket (make-inet-address "0.0.0.0") port)
    (socket-listen socket 64)
    socket))

(defun send-iac (stream command option)
  (write-char (code-char +iac+) stream)
  (write-char (code-char command) stream)
  (write-char (code-char option) stream)
  (finish-output stream))

(defun skip-subnegotiation (stream)
  (loop
    (let ((char (read-char stream nil nil)))
      (when (null char)
        (return))
      (when (and (= (char-code char) +iac+)
                 (let ((cmd (read-char stream nil nil)))
                   (when cmd
                     (= (char-code cmd) +se+))))
        (return)))))

(defun handle-iac (stream)
  (let ((command (read-char stream nil nil)))
    (cond
      ((null command) nil)
      ((= (char-code command) +iac+)
       (code-char +iac+))
      (t
       (let ((cmd-code (char-code command)))
         (cond
           ((member cmd-code (list +do+ +dont+ +will+ +wont+))
            (let ((option (read-char stream nil nil)))
              (when option
                (ecase cmd-code
                  (+do+ (send-iac stream +wont+ (char-code option)))
                  (+dont+ (send-iac stream +wont+ (char-code option)))
                  (+will+ (send-iac stream +dont+ (char-code option)))
                  (+wont+ (send-iac stream +dont+ (char-code option)))))
              nil))
           ((= cmd-code +sb+)
            (skip-subnegotiation stream)
            nil)
           (t nil)))))))

(defparameter *whitespace-chars* '(#\Space #\Tab #\Newline #\Return))

(defun skip-ansi-sequence (stream)
  "Skip ANSI escape sequences (arrow keys, etc.)"
  (let ((next-char (read-char stream nil nil)))
    (when next-char
      (cond
        ;; CSI sequences: ESC [ ... letter
        ((char= next-char #\[)
         (loop for ch = (read-char stream nil nil)
               while (and ch (not (alpha-char-p ch)))))
        ;; OSC sequences: ESC ] ... BEL or ST
        ((char= next-char #\])
         (loop for ch = (read-char stream nil nil)
               while (and ch (not (or (char= ch (code-char 7))
                                     (char= ch #\Backslash))))))
        ;; Other escape sequences - just read one more char
        (t nil)))))

(defun read-telnet-line (stream)
  (let ((buffer (make-string-output-stream)))
    (loop
      (let ((char (read-char stream nil nil)))
        (when (null char)
          (return-from read-telnet-line nil))
        (let ((code (char-code char)))
          (cond
            ((= code +lf+)
             (return (string-right-trim *whitespace-chars*
                                        (get-output-stream-string buffer))))
            ((= code +cr+)
             ;; Ignore bare carriage returns.
             nil)
            ((= code +iac+)
             (let ((result (handle-iac stream)))
               (when (characterp result)
                 (write-char result buffer))))
            ((= code 27)
             ;; ESC - skip ANSI escape sequence
             (skip-ansi-sequence stream))
            (t
             (write-char char buffer))))))))

(defun write-crlf (stream text)
  (write-string text stream)
  (write-char #\Return stream)
  (write-char #\Linefeed stream)
  (finish-output stream))

(defun prompt (stream)
  (write-string "> " stream)
  (finish-output stream))

(defun broadcast (message &optional except)
  (with-mutex (*clients-lock*)
    (dolist (client *clients*)
      (unless (eq client except)
        (let ((stream (player-stream client)))
          (when stream
            (ignore-errors (write-crlf stream message))))))))

(defun add-client (player)
  (with-mutex (*clients-lock*)
    (push player *clients*)))

(defun remove-client (player)
  (with-mutex (*clients-lock*)
    (setf *clients* (remove player *clients* :test #'eq))))

(defun unique-name-p (name)
  (with-mutex (*clients-lock*)
    (not (find name *clients* :test #'string-equal :key #'player-name))))

(defun sanitize-name (raw)
  (let* ((trimmed (string-trim *whitespace-chars* raw))
         (stripped (strip trimmed))
         (limited (subseq stripped 0 (min 20 (length stripped)))))
    (string-capitalize limited)))

(defun whitespace-char-p (ch)
  (member ch *whitespace-chars* :test #'char-equal))

(defun ask-for-name (stream)
  (loop
    (write-crlf stream (wrap "What is your name, traveler?" :bright-yellow))
    (write-string "> " stream)
    (finish-output stream)
    (let ((line (read-telnet-line stream)))
      (when (null line)
        (return-from ask-for-name nil))
      (let ((name (sanitize-name line)))
        (cond
          ((zerop (length name))
           (write-crlf stream (wrap "Names must contain at least one visible character." :bright-red)))
          ((not (unique-name-p name))
           (write-crlf stream (wrap "That name is already in use." :bright-red)))
          (t (return name)))))))

(defun current-room (player)
  (let ((room-id (player-room player)))
    (find-room room-id)))

(defun colorize-facets (text)
  "Replace [facet] markers with colorized text"
  (with-output-to-string (out)
    (let ((pos 0))
      (loop
        (let ((start (search "[" text :start2 pos)))
          (if (null start)
              (progn
                (write-string (subseq text pos) out)
                (return))
              (let ((end (search "]" text :start2 start)))
                (if (null end)
                    (progn
                      (write-string (subseq text pos) out)
                      (return))
                    (progn
                      ;; Write text before [
                      (write-string (subseq text pos start) out)
                      ;; Write colorized facet name
                      (let ((facet-name (subseq text (1+ start) end)))
                        (write-string (wrap facet-name :bright-yellow) out))
                      ;; Move position past ]
                      (setf pos (1+ end)))))))))))

(defun send-room-overview (player)
  (let* ((room-id (player-room player))
         (room (find-room room-id)))
    (format t "DEBUG: send-room-overview called for player ~a, room-id ~a, room ~a~%"
            (player-name player) room-id room)
    (finish-output)
    (if room
        (let ((stream (player-stream player)))
          ;; Show map first
          (write-crlf stream (wrap (generate-map (player-room player)) :bright-cyan))
          (write-crlf stream (wrap (format nil "~a" (room-name room)) :bold :bright-cyan))
          (write-crlf stream (colorize-facets (room-description room)))
          (write-crlf stream "")
          ;; Show other players in the room
          (with-mutex (*clients-lock*)
            (let ((others (remove-if (lambda (p) (or (eq p player)
                                                      (not (eq (player-room p) (player-room player)))))
                                     *clients*)))
              (when others
                (write-crlf stream (wrap (format nil "Also here: ~{~a~^, ~}"
                                                (mapcar #'player-name others))
                                        :bright-green)))))
          ;; Show items on the ground
          (let ((items-str (list-room-items (player-room player))))
            (when items-str
              (write-crlf stream (wrap (format nil "Items: ~a" items-str) :bright-white))))
          (write-crlf stream "")
          ;; Show exits based on vehicle type
          (let* ((vehicle-type (when (player-vehicle player)
                                 (mud.inventory::item-vehicle-type (player-vehicle player))))
                 (all-exits (room-exits room))
                 (available-exits
                   (remove-if-not
                     (lambda (exit-entry)
                       (let ((direction (car exit-entry))
                             (rest-of-entry (cdr exit-entry)))
                         ;; Check if this is a typed exit
                         (if (and (consp rest-of-entry) (keywordp (car rest-of-entry)))
                             ;; Typed exit - only show if we have matching vehicle
                             (and vehicle-type (eq vehicle-type (car rest-of-entry)))
                             ;; Simple exit - show if not in vehicle OR in uber vehicle
                             (or (null vehicle-type) (eq vehicle-type :uber)))))
                     all-exits))
                 (exit-names (mapcar (lambda (pair) (string-upcase (symbol-name (car pair))))
                                   available-exits)))
            (if exit-names
                (write-crlf stream (wrap (format nil "Exits: ~{~a~^, ~}" exit-names) :bright-yellow))
                (write-crlf stream (wrap "Exits: none" :bright-yellow))))
          ;; Show vehicle status
          (when (player-vehicle player)
            (write-crlf stream (wrap (format nil "You are in: ~a"
                                            (item-name (player-vehicle player)))
                                    :bright-cyan))))
        (write-crlf (player-stream player)
                    (wrap (format nil "ERROR: Room ~a not found!" room-id) :bright-red)))))

(defun move-player (player direction)
  (let* ((room (current-room player))
         (vehicle-type (when (player-vehicle player)
                         (mud.inventory::item-vehicle-type (player-vehicle player))))
         (target-id (and room (neighbor room direction vehicle-type)))
         (target (and target-id (find-room target-id))))
    (if (null target)
        (let ((stream (player-stream player)))
          (write-crlf stream (wrap "You can't go that way." :bright-red))
          nil)
        (progn
          (announce-to-room player
                             (format nil "~a slips ~a." (wrap (player-name player) :bright-blue)
                                     (string-downcase (symbol-name direction))))
          (set-player-room player (room-id target))
          (announce-to-room player
                             (format nil "~a arrives." (wrap (player-name player) :bright-green))
                             :include-self nil)
          (send-room-overview player)
          t))))

(defun announce-to-room (player message &key (include-self nil) (exclude nil))
  (let ((room-id (player-room player)))
    (with-mutex (*clients-lock*)
      (dolist (other *clients*)
        (when (and (eq (player-room other) room-id)
                   (or include-self (not (eq other player)))
                   (not (member other exclude)))
          (ignore-errors (write-crlf (player-stream other) message)))))))

(defun handle-say (player text)
  (let ((clean (string-trim '(#\Space #\Tab) text)))
    (if (zerop (length clean))
        (write-crlf (player-stream player) (wrap "Say what?" :bright-red))
        (announce-to-room player
                          (format nil "~a says: ~a"
                                  (wrap (player-name player) :bright-green)
                                  clean)
                          :include-self t))))

(defun find-player-by-name (name)
  "Find a connected player by name (case-insensitive)"
  (with-mutex (*clients-lock*)
    (find-if (lambda (p)
               (string-equal (player-name p) name))
             *clients*)))

(defun handle-cast (caster spell-and-target)
  "Handle casting a spell: cast <spell> <target>"
  (let* ((parts (split-on-whitespace spell-and-target))
         (spell-name (first parts))
         (target-name (second parts)))
    (cond
      ((null spell-name)
       (write-crlf (player-stream caster) (wrap "Cast what? Usage: cast <spell> <target>" :bright-red)))
      ((null target-name)
       (write-crlf (player-stream caster) (wrap "Cast at whom? Usage: cast <spell> <target>" :bright-red)))
      (t
       (let ((target (find-player-by-name target-name)))
         (cond
           ((null target)
            (write-crlf (player-stream caster)
                       (wrap (format nil "No player named '~a' is here." target-name) :bright-red)))
           ((not (eq (player-room caster) (player-room target)))
            (write-crlf (player-stream caster)
                       (wrap (format nil "~a is not in this room." (player-name target)) :bright-red)))
           (t
            (multiple-value-bind (success message death-occurred) (cast-spell caster target spell-name)
              (if success
                  (progn
                    (write-crlf (player-stream caster) (wrap message :bright-magenta))
                    (unless (eq caster target)
                      (write-crlf (player-stream target)
                                 (wrap (format nil "~a casts ~a at you!"
                                              (player-name caster) spell-name)
                                       :bright-red))
                      (announce-to-room caster
                                       (format nil "~a casts ~a at ~a!"
                                              (wrap (player-name caster) :bright-yellow)
                                              spell-name
                                              (wrap (player-name target) :bright-yellow))
                                       :exclude (list caster target)))
                    ;; Handle death
                    (when death-occurred
                      (write-crlf (player-stream target)
                                 (wrap "You have died! Your items have been left in a corpse." :bright-red))
                      (write-crlf (player-stream target)
                                 (wrap "You awaken in the graveyard, wounded but alive..." :bright-black))
                      (send-room-overview target)
                      (announce-to-room target
                                       (format nil "~a appears, looking worse for wear."
                                              (wrap (player-name target) :bright-green))
                                       :include-self nil)))
                  (write-crlf (player-stream caster) (wrap message :bright-red)))))))))))

(defun split-on-whitespace (text)
  "Split text on whitespace into a list of words"
  (let ((trimmed (string-trim '(#\Space #\Tab) text)))
    (if (zerop (length trimmed))
        nil
        (loop with start = 0
              with len = (length trimmed)
              while (< start len)
              for end = (or (position-if (lambda (c) (or (char= c #\Space) (char= c #\Tab)))
                                         trimmed :start start)
                           len)
              collect (subseq trimmed start end)
              do (setf start (position-if-not (lambda (c) (or (char= c #\Space) (char= c #\Tab)))
                                             trimmed :start end))
              while start))))

(defun parse-command (line)
  (let* ((trimmed (string-trim *whitespace-chars* line)))
    (if (zerop (length trimmed))
        (values nil "")
        (let ((split (position-if #'whitespace-char-p trimmed)))
          (if split
              (values (string-downcase (subseq trimmed 0 split))
                      (string-left-trim *whitespace-chars*
                                        (subseq trimmed split)))
              (values (string-downcase trimmed) ""))))))

(defun normalize-direction (dir-string)
  "Convert direction string to keyword, handling single-letter aliases"
  (let ((dir (string-downcase dir-string)))
    (cond
      ((string= dir "n") :north)
      ((string= dir "s") :south)
      ((string= dir "e") :east)
      ((string= dir "w") :west)
      ((string= dir "u") :up)
      ((string= dir "d") :down)
      ((string= dir "ne") :northeast)
      ((string= dir "nw") :northwest)
      ((string= dir "se") :southeast)
      ((string= dir "sw") :southwest)
      (t (intern (string-upcase dir) :keyword)))))

(defun handle-command (player line)
  (multiple-value-bind (verb rest) (parse-command line)
    (cond
      ((null verb) (send-room-overview player))
      ((member verb '("look" "l") :test #'string=)
       (if (zerop (length rest))
           ;; Look at room
           (send-room-overview player)
           ;; Look at specific target
           (let ((target-name (string-trim '(#\Space #\Tab) rest)))
             ;; First check if it's a player in the room
             (let ((target-player nil))
               (with-mutex (*clients-lock*)
                 (setf target-player
                       (find-if (lambda (p)
                                  (and (not (eq p player))
                                       (eq (player-room p) (player-room player))
                                       (string-equal (player-name p) target-name)))
                                *clients*)))
               (cond
                 ;; Found a player
                 (target-player
                  (write-crlf (player-stream player)
                             (wrap (format nil "~a stands here. ~a"
                                          (player-name target-player)
                                          (get-player-stats target-player))
                                   :bright-green)))
                 ;; Check for a facet in the room
                 (t
                  (let ((facet (find-facet-in-room (player-room player) target-name)))
                    (if facet
                        (write-crlf (player-stream player)
                                   (wrap (cdr facet) :bright-magenta))
                        ;; Check for an item in the room
                        (let ((room-item (find-item-in-room (player-room player) target-name)))
                          (if room-item
                              (write-crlf (player-stream player)
                                         (wrap (format nil "~a: ~a"
                                                      (item-name room-item)
                                                      (mud.inventory::item-description room-item))
                                               :bright-white))
                              ;; Check for an item in inventory
                              (let ((inv-item (find-in-inventory player target-name)))
                                (if inv-item
                                    (write-crlf (player-stream player)
                                               (wrap (format nil "~a (in your inventory): ~a"
                                                            (item-name inv-item)
                                                            (mud.inventory::item-description inv-item))
                                                     :bright-cyan))
                                    (write-crlf (player-stream player)
                                               (wrap (format nil "You don't see '~a' here." target-name)
                                                     :bright-red))))))))))))))
      ((member verb '("move" "go") :test #'string=)
       (if (zerop (length rest))
           (write-crlf (player-stream player) (wrap "Go where?" :bright-red))
           (let* ((dir-token (subseq rest 0 (or (position-if #'whitespace-char-p rest) (length rest))))
                  (keyword (normalize-direction dir-token)))
             (move-player player keyword))))
      ((member verb '("n" "s" "e" "w" "u" "d" "ne" "nw" "se" "sw" "downstream" "upstream") :test #'string=)
       (move-player player (normalize-direction verb)))
      ((string= verb "say")
       (handle-say player rest))
      ((string= verb "who")
       (with-mutex (*clients-lock*)
         (let ((names (mapcar #'player-name *clients*)))
           (write-crlf (player-stream player)
                       (wrap (if names
                                 (format nil "Wanderers about: ~{~a~^, ~}" names)
                                 "You are alone in the twilight.")
                             :bright-magenta)))))
      ((string= verb "stats")
       (write-crlf (player-stream player)
                   (wrap (get-player-stats player) :bright-cyan)))
      ((string= verb "spells")
       (write-crlf (player-stream player)
                   (wrap "Available spells:" :bright-yellow))
       (dolist (spell *spells*)
         (write-crlf (player-stream player)
                     (format nil "  ~a (~d mana, ~a dmg) - ~a"
                            (wrap (spell-name spell) :bright-magenta)
                            (spell-cost spell)
                            (if (minusp (spell-damage spell))
                                (format nil "heals ~d" (abs (spell-damage spell)))
                                (spell-damage spell))
                            (spell-description spell)))))
      ((string= verb "cast")
       (handle-cast player rest))
      ((member verb '("inventory" "inv" "i") :test #'string=)
       (write-crlf (player-stream player) (list-inventory player)))
      ((string= verb "use")
       (if (zerop (length rest))
           (write-crlf (player-stream player) (wrap "Use what? Usage: use <item>" :bright-red))
           (let ((item-name (string-trim '(#\Space #\Tab) rest)))
             (multiple-value-bind (success message) (use-item player item-name)
               (if success
                   (write-crlf (player-stream player) (wrap message :bright-green))
                   (write-crlf (player-stream player) (wrap message :bright-red)))))))
      ((string= verb "drop")
       (if (zerop (length rest))
           (write-crlf (player-stream player) (wrap "Drop what? Usage: drop <item>" :bright-red))
           (let ((item-name (string-trim '(#\Space #\Tab) rest)))
             (multiple-value-bind (success message) (drop-item player item-name)
               (if success
                   (progn
                     (write-crlf (player-stream player) (wrap message :bright-green))
                     (announce-to-room player
                                      (format nil "~a drops ~a."
                                             (wrap (player-name player) :bright-yellow)
                                             item-name)
                                      :include-self nil))
                   (write-crlf (player-stream player) (wrap message :bright-red)))))))
      ((member verb '("get" "grab") :test #'string=)
       (if (zerop (length rest))
           (write-crlf (player-stream player) (wrap "Get what? Usage: get <item>" :bright-red))
           (let ((item-name (string-trim '(#\Space #\Tab) rest)))
             ;; Check if it's a corpse
             (let ((item (find-item-in-room (player-room player) item-name)))
               (if (and item (eq (item-type item) :corpse))
                   ;; Looting a corpse
                   (let ((corpse-items (loot-corpse item)))
                     (if corpse-items
                         (progn
                           (dolist (corpse-item corpse-items)
                             (add-to-inventory player corpse-item))
                           (remove-item-from-room (player-room player) item)
                           (write-crlf (player-stream player)
                                      (wrap (format nil "You loot the corpse and take ~d item~:p."
                                                   (length corpse-items))
                                            :bright-green))
                           (announce-to-room player
                                            (format nil "~a loots ~a."
                                                   (wrap (player-name player) :bright-yellow)
                                                   item-name)
                                            :include-self nil))
                         (write-crlf (player-stream player) (wrap "The corpse is empty." :bright-red))))
                   ;; Normal item pickup
                   (multiple-value-bind (success message) (grab-item player item-name)
                     (if success
                         (progn
                           (write-crlf (player-stream player) (wrap message :bright-green))
                           (announce-to-room player
                                            (format nil "~a gets ~a."
                                                   (wrap (player-name player) :bright-yellow)
                                                   item-name)
                                            :include-self nil))
                         (write-crlf (player-stream player) (wrap message :bright-red)))))))))
      ((string= verb ".")
       (write-crlf (player-stream player) (wrap "No previous command to repeat." :bright-red)))
      ((string= verb "suicide")
       ;; Test command to trigger death mechanics
       (write-crlf (player-stream player) (wrap "You take your own life..." :bright-red))
       (announce-to-room player
                        (format nil "~a falls to the ground, lifeless."
                               (wrap (player-name player) :bright-red))
                        :include-self nil)
       (format t "DEBUG: Before handle-player-death, room is ~a~%" (player-room player))
       (finish-output)
       (handle-player-death player)
       (format t "DEBUG: After handle-player-death, room is ~a~%" (player-room player))
       (finish-output)
       (write-crlf (player-stream player)
                  (wrap "You have died! Your items have been left in a corpse." :bright-red))
       (write-crlf (player-stream player)
                  (wrap "You awaken in the graveyard, wounded but alive..." :bright-black))
       (send-room-overview player)
       (announce-to-room player
                        (format nil "~a appears, looking worse for wear."
                               (wrap (player-name player) :bright-green))
                        :include-self nil))
      ((string= verb "enter")
       (if (zerop (length rest))
           (write-crlf (player-stream player) (wrap "Enter what?" :bright-red))
           (let* ((target-name (string-trim '(#\Space #\Tab) rest))
                  (vehicle-item (find-item-in-room (player-room player) target-name)))
             (cond
               ((player-vehicle player)
                (write-crlf (player-stream player)
                           (wrap (format nil "You are already in ~a."
                                        (item-name (player-vehicle player)))
                                 :bright-red)))
               ((or (null vehicle-item) (not (eq (item-type vehicle-item) :vehicle)))
                (write-crlf (player-stream player)
                           (wrap (format nil "You can't enter '~a'." target-name) :bright-red)))
               (t
                ;; Remove vehicle from room, store in player
                (remove-item-from-room (player-room player) vehicle-item)
                (setf (player-vehicle player) vehicle-item)
                (write-crlf (player-stream player)
                           (wrap (format nil "You enter ~a." (item-name vehicle-item))
                                 :bright-cyan))
                (announce-to-room player
                                 (format nil "~a enters ~a."
                                        (wrap (player-name player) :bright-blue)
                                        (item-name vehicle-item))
                                 :include-self nil)
                (send-room-overview player))))))
      ((string= verb "exit")
       (cond
         ((null (player-vehicle player))
          (write-crlf (player-stream player)
                     (wrap "You are not in a vehicle." :bright-red)))
         (t
          (let ((vehicle-item (player-vehicle player)))
            ;; Put vehicle back in the room
            (add-item-to-room (player-room player) vehicle-item)
            (setf (player-vehicle player) nil)
            (write-crlf (player-stream player)
                       (wrap (format nil "You exit ~a." (item-name vehicle-item)) :bright-cyan))
            (announce-to-room player
                             (format nil "~a exits ~a."
                                    (wrap (player-name player) :bright-blue)
                                    (item-name vehicle-item))
                             :include-self nil)
            (send-room-overview player)))))
      ((string= verb "uber")
       (cond
         ((null (player-vehicle player))
          (write-crlf (player-stream player)
                     (wrap "You need to be in a vehicle to use uber." :bright-red)))
         ((not (eq (mud.inventory::item-vehicle-type (player-vehicle player)) :uber))
          (write-crlf (player-stream player)
                     (wrap "This vehicle doesn't support uber travel." :bright-red)))
         ((zerop (length rest))
          (write-crlf (player-stream player)
                     (wrap "Uber to where? Usage: uber <location name>" :bright-red)))
         (t
          (let* ((destination-name (string-trim '(#\Space #\Tab) rest))
                 (destination-room (find-room-by-name destination-name)))
            (if destination-room
                (progn
                  (write-crlf (player-stream player)
                             (wrap (format nil "The ~a shimmers with energy and instantly transports you to ~a!"
                                          (item-name (player-vehicle player))
                                          (mud.world::room-name destination-room))
                                   :bright-magenta))
                  (announce-to-room player
                                   (format nil "~a vanishes in a flash of light!"
                                          (wrap (player-name player) :bright-blue))
                                   :include-self nil)
                  (set-player-room player (mud.world::room-id destination-room))
                  (send-room-overview player)
                  (announce-to-room player
                                   (format nil "~a appears in a flash of light, riding in ~a!"
                                          (wrap (player-name player) :bright-green)
                                          (item-name (player-vehicle player)))
                                   :include-self nil))
                (write-crlf (player-stream player)
                           (wrap (format nil "Location '~a' not found." destination-name)
                                 :bright-red)))))))
      ((string= verb "help")
       (write-crlf (player-stream player)
                   (wrap "Commands:" :bright-yellow))
       (write-crlf (player-stream player) "  Movement: look (l), go <dir> (n/s/e/w/u/d/ne/nw/se/sw), enter <vehicle>, exit, uber <location>")
       (write-crlf (player-stream player) "  Social: say <text>, who")
       (write-crlf (player-stream player) "  Combat: cast <spell> <target>, stats, spells")
       (write-crlf (player-stream player) "  Inventory: inventory (inv/i), use <item>, drop <item>, get <item> (loot corpses)")
       (write-crlf (player-stream player) "  Other: help, quit, . (repeat last command), suicide (test death)"))
      ((member verb '("quit" "exit") :test #'string=)
       :quit)
      (t
       (write-crlf (player-stream player) (wrap "Unknown command." :bright-red))))))

(defun client-loop (socket stream)
  (let ((player nil)
        (graceful nil))
    (unwind-protect
         (let ((name (ask-for-name stream)))
           (unless name
             (return-from client-loop))
           (let* ((start (starting-room))
                  (room-id (room-id start)))
             (setf player (make-player :name name :room room-id :stream stream :socket socket))
             ;; Give starting inventory: 3 mana potions
             (dotimes (i 3)
               (add-to-inventory player (create-item "mana-potion")))
             (add-client player)
             (write-crlf stream (wrap "Welcome to the Endless Evening." :bright-cyan))
             (announce-to-room player (format nil "~a arrives." (wrap name :bright-green)) :include-self nil)
             (send-room-overview player)
             (let ((last-command nil))
               (loop
                 (prompt stream)
                 (let ((line (read-telnet-line stream)))
                   (when (null line)
                     (return))
                   ;; Handle '.' repeat command
                   (when (and (string= line ".") last-command)
                     (setf line last-command))
                   ;; Store command for repeat (unless it's a '.')
                   (unless (string= line ".")
                     (setf last-command line))
                   (case (handle-command player line)
                     (:quit
                      (setf graceful t)
                      (write-crlf stream (wrap "May the stars guide your steps." :bright-yellow))
                      (return)))))))))
      (when player
        (announce-to-room player
                          (if graceful
                              (format nil "~a fades into the night." (wrap (player-name player) :bright-blue))
                              (format nil "~a disconnects." (wrap (player-name player) :bright-red)))))
      (when player
        (remove-client player))
      (ignore-errors (close stream))
      (ignore-errors (close socket))))

(defun accept-loop (socket)
  (loop while *running* do
    (handler-case
        (let* ((client (socket-accept socket))
               (stream (socket-make-stream client :input t :output t :element-type 'character :external-format :latin-1 :buffering :line)))
          (make-thread #'client-loop :name "mud-client" :arguments (list client stream)))
      (socket-error (err)
        (when *running*
          (server-log "Socket error: ~a" err))))))

(defun start (&key (port 4000))
  (when *running*
    (error "Server already running."))
  (initialize-world)
  (setf *running* t)
  (let ((socket (make-server-socket port)))
    (setf *listener* socket)
    (setf *listener-thread*
          (make-thread (lambda () (accept-loop socket)) :name "mud-acceptor"))
    (server-log "MUD listening on port ~a" port)
    *listener*))

(defun stop ()
  (when *listener*
    (setf *running* nil)
    (ignore-errors (close *listener*))
    (setf *listener* nil)
    (when *listener-thread*
      (join-thread *listener-thread*)
      (setf *listener-thread* nil))
    (with-mutex (*clients-lock*)
      (dolist (client *clients*)
        (ignore-errors (close (player-stream client)))
        (ignore-errors (close (player-socket client))))
      (setf *clients* '()))
    (server-log "MUD server stopped.")))

(defun await ()
  (when *listener-thread*
    (join-thread *listener-thread*)))
