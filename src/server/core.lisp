(in-package :mud.server)


(declaim
 (ftype (function (t t &key (:include-self t) (:exclude t)) t)
  announce-to-room))


(defparameter *listener* nil)


(defparameter *listener-thread* nil)


(defparameter *clients* 'nil)


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
     (when (null char) (return))
     (when
         (and (= (char-code char) +iac+)
              (let ((cmd (read-char stream nil nil)))
                (when cmd (= (char-code cmd) +se+))))
       (return)))))


(defun handle-iac (stream)
  (let ((command (read-char stream nil nil)))
    (cond ((null command) nil)
          ((= (char-code command) +iac+) (code-char +iac+))
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
              ((= cmd-code +sb+) (skip-subnegotiation stream) nil) (t nil)))))))


(defparameter *whitespace-chars* '(#\  #\Tab #\Newline #\Return))


(defun skip-ansi-sequence (stream)
  "Skip ANSI escape sequences (arrow keys, etc.)"
  (let ((next-char (read-char stream nil nil)))
    (when next-char
      (cond
       ((char= next-char #\[)
        (loop for ch = (read-char stream nil nil)
              while (and ch (not (alpha-char-p ch)))))
       ((char= next-char #\])
        (loop for ch = (read-char stream nil nil)
              while (and ch
                         (not (or (char= ch (code-char 7)) (char= ch #\\))))))
       (t nil)))))


(defun read-telnet-line (stream)
  (let ((buffer (make-string-output-stream)))
    (loop
     (let ((char (read-char stream nil nil)))
       (when (null char) (return-from read-telnet-line nil))
       (let ((code (char-code char)))
         (cond
          ((= code +lf+)
           (return
            (string-right-trim *whitespace-chars*
                               (get-output-stream-string buffer))))
          ((= code +cr+) nil)
          ((= code +iac+)
           (let ((result (handle-iac stream)))
             (when (characterp result) (write-char result buffer))))
          ((= code 27) (skip-ansi-sequence stream))
          (t (write-char char buffer))))))))


(defun write-crlf (stream text)
  (write-string text stream)
  (write-char #\Return stream)
  (write-char #\Newline stream)
  (finish-output stream))


(defun prompt (stream) (write-string "> " stream) (finish-output stream))


(defun broadcast (message &optional except)
  (with-mutex (*clients-lock*)
   (dolist (client *clients*)
     (unless (eq client except)
       (let ((stream (player-stream client)))
         (when stream (ignore-errors (write-crlf stream message))))))))


(defun add-client (player)
  (with-mutex (*clients-lock*) (push player *clients*)))


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


(defun whitespace-char-p (ch) (member ch *whitespace-chars* :test #'char-equal))


(defun ask-for-name (stream)
  (loop
   (write-crlf stream (wrap "What is your name, traveler?" :bright-yellow))
   (write-string "> " stream)
   (finish-output stream)
   (let ((line (read-telnet-line stream)))
     (when (null line) (return-from ask-for-name nil))
     (let ((name (sanitize-name line)))
       (cond
        ((zerop (length name))
         (write-crlf stream
          (wrap "Names must contain at least one visible character."
           :bright-red)))
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
             (progn (write-string (subseq text pos) out) (return))
             (let ((end (search "]" text :start2 start)))
               (if (null end)
                   (progn (write-string (subseq text pos) out) (return))
                   (progn
                    (write-string (subseq text pos start) out)
                    (let ((facet-name (subseq text (1+ start) end)))
                      (write-string (wrap facet-name :bright-yellow) out))
                    (setf pos (1+ end)))))))))))


(defun handle-look-at (player rest)
  "Handle looking at a specific target"
  (let* ((target-name (string-trim '(#\  #\Tab) rest))
         (stream (player-stream player)))
    (let ((target-player nil))
      (with-mutex (*clients-lock*)
        (setf target-player
              (find-if
               (lambda (p)
                 (and (not (eq p player))
                      (eq (player-room p) (player-room player))
                      (string-equal (player-name p) target-name)))
               *clients*)))
      (when target-player
        (write-crlf stream
         (wrap
          (format nil "~a stands here. ~a" (player-name target-player)
                  (get-player-stats target-player))
          :bright-green))
        (return-from handle-look-at nil)))
    (let ((merchant (find-merchant-in-room-by-name (player-room player)
                                                   target-name)))
      (when merchant
        (write-crlf stream (wrap (merchant-name merchant) :bright-yellow))
        (when (merchant-description merchant)
          (write-crlf stream
           (wrap (merchant-description merchant) :bright-white)))
        (when (merchant-greeting merchant)
          (write-crlf stream (wrap (merchant-greeting merchant) :bright-black)))
        (let ((summary (merchant-stock-summary merchant)))
          (when summary
            (write-crlf stream (wrap summary :bright-cyan))))
        (write-crlf stream
         (wrap "Use 'shop' to browse, then 'buy <item>' or 'sell <item>'."
               :bright-magenta))
        (return-from handle-look-at nil)))
    (let ((mob (find-mob-in-room (player-room player) target-name)))
      (when mob
        (write-crlf stream
         (wrap
          (format nil "~a: ~a~%Health: ~d/~d  Damage: ~d  Armor: ~d"
                  (mob-name mob) (mob-description mob)
                  (mob-health mob) (mob-max-health mob)
                  (mob-damage mob) (mob-armor mob))
          :bright-red))
        (return-from handle-look-at nil)))
    (let ((facet (find-facet-in-room (player-room player) target-name)))
      (when facet
        (write-crlf stream (wrap (cdr facet) :bright-magenta))
        (return-from handle-look-at nil)))
    (let ((room-item (find-item-in-room (player-room player) target-name)))
      (when room-item
        (write-crlf stream
         (wrap (format nil "~a: ~a" (item-name room-item)
                       (mud.inventory:item-description room-item))
               :bright-white))
        (when (eq (item-type room-item) :corpse)
          (let ((corpse-contents
                 (gethash (item-name room-item) mud.combat:*corpse-data*)))
            (if corpse-contents
                (let ((counts (make-hash-table :test #'equal)))
                  (dolist (it corpse-contents)
                    (incf (gethash (mud.inventory:item-name it) counts 0)))
                  (let ((contents-str
                         (with-output-to-string (out)
                           (let ((first t))
                             (maphash
                              (lambda (name count)
                                (unless first (format out ", "))
                                (setf first nil)
                                (if (> count 1)
                                    (format out "~a (x~d)" name count)
                                    (format out "~a" name)))
                              counts)))))
                    (write-crlf stream
                     (wrap (format nil "Contents: ~a" contents-str)
                           :bright-yellow))))
                (write-crlf stream
                 (wrap "The corpse is empty." :bright-black)))))
        (return-from handle-look-at nil)))
    (let ((inv-item (find-in-inventory player target-name)))
      (if inv-item
          (write-crlf stream
           (wrap
            (format nil "~a (in your inventory): ~a"
                    (item-name inv-item)
                    (mud.inventory:item-description inv-item))
            :bright-cyan))
          (let ((equipped-item (mud.inventory:find-equipped-item player target-name)))
            (if equipped-item
                (let ((item-type (mud.inventory:item-type equipped-item))
                      (damage (mud.inventory:item-damage equipped-item))
                      (armor (mud.inventory:item-armor equipped-item)))
                  (write-crlf stream
                   (wrap
                    (format nil "~a (equipped): ~a"
                            (mud.inventory:item-name equipped-item)
                            (mud.inventory:item-description equipped-item))
                    :bright-green))
                  (when (and (eq item-type :weapon) (> damage 0))
                    (write-crlf stream
                     (wrap (format nil "Damage bonus: +~d" damage) :bright-yellow)))
                  (when (and (eq item-type :armor) (> armor 0))
                    (write-crlf stream
                     (wrap (format nil "Armor bonus: +~d" armor) :bright-yellow))))
                (write-crlf stream
                 (wrap (format nil "You don't see '~a' here." target-name)
                       :bright-red))))))))


(defun send-room-overview (player)
  (let* ((room-id (player-room player))
         (room (find-room room-id))
         (vehicle (player-vehicle player))
         (vehicle-type (when vehicle
                         (mud.inventory:item-vehicle-type vehicle))))
    (if room
        (let ((stream (player-stream player)))
          (write-crlf stream
           (wrap (generate-map room-id vehicle-type) :bright-cyan))
          (write-crlf stream
           (wrap (format nil "~a" (room-name room)) :bold :bright-cyan))
          (write-crlf stream (colorize-facets (room-description room)))
          (write-crlf stream "")
          (with-mutex (*clients-lock*)
           (let ((others
                  (remove-if
                   (lambda (p)
                     (or (eq p player)
                         (not (eq (player-room p) (player-room player)))))
                   *clients*)))
             (when others
               (write-crlf stream
                (wrap
                 (format nil "Also here: ~{~a~^, ~}"
                         (mapcar #'player-name others))
                 :bright-green)))))
          (let ((merchants (get-merchants-in-room (player-room player))))
            (when merchants
              (write-crlf stream
               (wrap (format nil "Merchants: ~{~a~^, ~}"
                              (mapcar #'merchant-name merchants))
                     :bright-yellow))
              (dolist (merchant merchants)
                (let ((greeting (merchant-greeting merchant)))
                  (when greeting
                    (write-crlf stream (wrap greeting :bright-black)))))))
          (let ((mobs (get-mobs-in-room (player-room player))))
            (when mobs
              (write-crlf stream
               (wrap (format nil "Mobs: ~{~a~^, ~}" (mapcar #'mob-name mobs))
                :bright-red))))
          (let ((items-str (list-room-items (player-room player))))
            (when items-str
              (write-crlf stream
               (wrap (format nil "Items: ~a" items-str) :bright-white))))
          (write-crlf stream "")
          (let* ((all-exits (room-exits room))
                 (available-exits
                  (remove-if-not
                   (lambda (exit-entry)
                     (neighbor room (car exit-entry) vehicle-type))
                   all-exits))
                 (exit-names
                  (mapcar
                   (lambda (pair) (string-upcase (symbol-name (car pair))))
                   available-exits)))
            (if exit-names
                (write-crlf stream
                 (wrap (format nil "Exits: ~{~a~^, ~}" exit-names)
                  :bright-yellow))
                (write-crlf stream (wrap "Exits: none" :bright-yellow))))
          (when vehicle
            (write-crlf stream
             (wrap
              (format nil "You are in: ~a" (item-name vehicle))
              :bright-cyan))))
        (write-crlf (player-stream player)
         (wrap (format nil "ERROR: Room ~a not found!" room-id) :bright-red)))))


(defun announce-to-room (player message &key (include-self nil) (exclude nil))
  (let ((room-id (player-room player)))
    (with-mutex (*clients-lock*)
     (dolist (other *clients*)
       (when
           (and (eq (player-room other) room-id)
                (or include-self (not (eq other player)))
                (not (member other exclude)))
         (ignore-errors (write-crlf (player-stream other) message)))))))


(defun move-player (player direction)
  (let* ((room (current-room player))
         (vehicle-type
          (when (player-vehicle player)
            (mud.inventory:item-vehicle-type (player-vehicle player))))
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


(defun handle-say (player text)
  (let ((clean (string-trim '(#\  #\Tab) text)))
    (if (zerop (length clean))
        (write-crlf (player-stream player) (wrap "Say what?" :bright-red))
        (announce-to-room player
         (format nil "~a says: ~a" (wrap (player-name player) :bright-green)
                 clean)
         :include-self t))))


(defun find-player-by-name (name)
  "Find a connected player by name (case-insensitive)"
  (with-mutex (*clients-lock*)
   (find-if (lambda (p) (string-equal (player-name p) name)) *clients*)))


(defun resolve-mob-hit (player mob damage)
  "Apply DAMAGE to MOB from PLAYER and handle death/xp/counter-attacks."
  (let ((mob-died (damage-mob mob damage)))
    (cond
     (mob-died
      (write-crlf (player-stream player)
       (wrap (format nil "You have slain ~a!" (mob-name mob)) :bright-green))
      (announce-to-room player
       (format nil "~a has slain ~a!" (wrap (player-name player) :bright-green)
               (mob-name mob))
       :include-self nil)
      (let ((xp (mob-xp-reward mob)))
        (let ((leveled-up (award-xp player xp)))
          (write-crlf (player-stream player)
           (wrap (format nil "You gained ~d XP!" xp) :bright-cyan))
          (when leveled-up
            (write-crlf (player-stream player)
             (wrap
              (format nil "*** LEVEL UP! You are now level ~d! ***"
                      (player-level player))
              :bright-magenta))
            (write-crlf (player-stream player)
             (wrap
              (format nil "Health: +10 (now ~d)  Mana: +5 (now ~d)"
                      (player-max-health player) (player-max-mana player))
              :bright-green)))))
      (let ((loot (get-mob-loot mob)))
        (when loot
          (dolist (item loot) (add-item-to-room (player-room player) item))
          (write-crlf (player-stream player)
           (wrap
            (format nil "~a dropped: ~{~a~^, ~}" (mob-name mob)
                    (mapcar #'item-name loot))
            :bright-yellow))))
      (remove-mob-from-room (player-room player) mob) t)
     (t
      (write-crlf (player-stream player)
       (format nil "~a has ~d/~d health remaining." (mob-name mob)
               (mob-health mob) (mob-max-health mob)))
      (let* ((mob-dmg (mob-damage mob))
             (player-armor (get-player-armor player))
             (counter-damage (max 1 (- mob-dmg player-armor))))
        (modify-health player (- counter-damage))
        (write-crlf (player-stream player)
         (wrap
          (format nil "~a attacks you for ~d damage!" (mob-name mob)
                  counter-damage)
          :bright-red))
        (announce-to-room player
         (format nil "~a is attacked by ~a!"
                 (wrap (player-name player) :bright-red) (mob-name mob))
         :include-self nil)
        (unless (player-alive-p player)
          (write-crlf (player-stream player)
           (wrap "You have been slain!" :bright-red))
          (announce-to-room player
           (format nil "~a has been slain by ~a!"
                   (wrap (player-name player) :bright-red) (mob-name mob))
           :include-self nil)
          (handle-player-death player)
          (write-crlf (player-stream player)
           (wrap "You awaken in the graveyard, wounded but alive..."
            :bright-black))
          (send-room-overview player)))))))


(defun split-on-whitespace (string)
  "Split STRING on whitespace"
  (uiop:split-string string :separator '(#\Space #\Tab #\Newline)))

(defun cast-spell-at-mob (caster mob spell)
  "Cast SPELL from CASTER at MOB, handling mana, damage, and outcomes."
  (cond
   ((not (player-alive-p caster))
    (write-crlf (player-stream caster)
     (wrap "You are dead and cannot cast spells." :bright-red)))
   ((not (mob-alive-p mob))
    (write-crlf (player-stream caster)
     (wrap (format nil "~a is already dead." (mob-name mob)) :bright-red)))
   ((minusp (spell-damage spell))
    (write-crlf (player-stream caster)
     (wrap "That spell can't target mobs." :bright-red)))
   ((< (player-mana caster) (spell-cost spell))
    (write-crlf (player-stream caster)
     (wrap
      (format nil "Not enough mana. ~a costs ~d mana." (spell-name spell)
              (spell-cost spell))
      :bright-red)))
   (t (modify-mana caster (- (spell-cost spell)))
    (let ((damage (spell-damage spell)))
      (write-crlf (player-stream caster)
       (wrap
        (format nil "You cast ~a at ~a for ~d damage!" (spell-name spell)
                (mob-name mob) damage)
        :bright-magenta))
      (announce-to-room caster
       (format nil "~a casts ~a at ~a!"
               (wrap (player-name caster) :bright-yellow) (spell-name spell)
               (mob-name mob))
       :include-self nil)
      (resolve-mob-hit caster mob damage)))))


(defun handle-cast (caster spell-and-target)
  "Handle casting a spell: cast <spell> <target>"
  (let* ((parts (split-on-whitespace spell-and-target))
         (spell-name (first parts))
         (target-name (second parts)))
    (cond
     ((null spell-name)
      (write-crlf (player-stream caster)
       (wrap "Cast what? Usage: cast <spell> <target>" :bright-red)))
     ((null target-name)
      (write-crlf (player-stream caster)
       (wrap "Cast at whom? Usage: cast <spell> <target>" :bright-red)))
     (t
      (let ((spell (find-spell spell-name)))
        (if (null spell)
            (write-crlf (player-stream caster)
             (wrap (format nil "Unknown spell: ~a" spell-name) :bright-red))
            (let ((target (find-player-by-name target-name)))
              (cond
               (target
                (if (eq (player-room caster) (player-room target))
                    (multiple-value-bind (success message death-occurred)
                        (cast-spell caster target spell-name)
                      (if success
                          (progn
                           (write-crlf (player-stream caster)
                            (wrap message :bright-magenta))
                           (unless (eq caster target)
                             (write-crlf (player-stream target)
                              (wrap
                               (format nil "~a casts ~a at you!"
                                       (player-name caster) spell-name)
                               :bright-red))
                             (announce-to-room caster
                              (format nil "~a casts ~a at ~a!"
                                      (wrap (player-name caster)
                                       :bright-yellow)
                                      spell-name
                                      (wrap (player-name target)
                                       :bright-yellow))
                              :exclude (list caster target)))
                           (when death-occurred
                             (write-crlf (player-stream target)
                              (wrap
                               "You have died! Your items have been left in a corpse."
                               :bright-red))
                             (write-crlf (player-stream target)
                              (wrap
                               "You awaken in the graveyard, wounded but alive..."
                               :bright-black))
                             (send-room-overview target)
                             (announce-to-room target
                              (format nil "~a appears, looking worse for wear."
                                      (wrap (player-name target)
                                       :bright-green))
                              :include-self nil)))
                          (write-crlf (player-stream caster)
                           (wrap message :bright-red))))
                    (write-crlf (player-stream caster)
                     (wrap
                      (format nil "~a is not in this room."
                              (player-name target))
                      :bright-red))))
               (t
                (let ((mob (find-mob-in-room (player-room caster) target-name)))
                  (if mob
                      (cast-spell-at-mob caster mob spell)
                      (write-crlf (player-stream caster)
                       (wrap
                        (format nil "There is no ~a here to target."
                                target-name)
                        :bright-red)))))))))))))


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
    (cond ((string= dir "n") :north) ((string= dir "s") :south)
          ((string= dir "e") :east) ((string= dir "w") :west)
          ((string= dir "u") :up) ((string= dir "d") :down)
          ((string= dir "ne") :northeast) ((string= dir "nw") :northwest)
          ((string= dir "se") :southeast) ((string= dir "sw") :southwest)
          (t (intern (string-upcase dir) :keyword)))))


