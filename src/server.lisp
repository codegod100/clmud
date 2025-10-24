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

(defun log (fmt &rest args)
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
            (t
             (write-char char buffer)))))))

(defun write-crlf (stream text)
  (write-string text stream)
  (write-string "\r\n" stream)
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

(defun send-room-overview (player)
  (let ((room (current-room player)))
    (when room
      (let ((stream (player-stream player)))
        (write-crlf stream
                    (wrap (format nil "~a" (room-name room)) :bold :bright-cyan))
        (write-crlf stream (room-description room))
        (write-crlf stream "")
        (let ((exits (mapcar (lambda (pair) (string-upcase (symbol-name (car pair)))) (room-exits room))))
          (write-crlf stream
                      (wrap (format nil "Exits: ~{~a~^, ~}" exits) :bright-yellow))))))

(defun move-player (player direction)
  (let* ((room (current-room player))
         (target-id (and room (neighbor room direction)))
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

(defun announce-to-room (player message &key (include-self nil))
  (let ((room-id (player-room player)))
    (with-mutex (*clients-lock*)
      (dolist (other *clients*)
        (when (and (eq (player-room other) room-id)
                   (or include-self (not (eq other player))))
          (ignore-errors (write-crlf (player-stream other) message))))))))

(defun handle-say (player text)
  (let ((clean (string-trim '(#\Space #\Tab) text)))
    (if (zerop (length clean))
        (write-crlf (player-stream player) (wrap "Say what?" :bright-red))
        (progn
          (announce-to-room player
                             (format nil "~a says: ~a"
                                     (wrap (player-name player) :bright-green)
                                     clean)
                             :include-self t)
          t))))

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

(defun handle-command (player line)
  (multiple-value-bind (verb rest) (parse-command line)
    (cond
      ((null verb) (send-room-overview player))
      ((member verb '("look" "l") :test #'string=)
       (send-room-overview player))
      ((member verb '("move" "go") :test #'string=)
       (if (zerop (length rest))
           (write-crlf (player-stream player) (wrap "Go where?" :bright-red))
           (let* ((dir-token (subseq rest 0 (or (position-if #'whitespace-char-p rest) (length rest))))
                  (keyword (intern (string-upcase dir-token) :keyword)))
             (move-player player keyword))))
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
      ((string= verb "help")
       (write-crlf (player-stream player)
                   (wrap "Commands: look, go <dir>, say <text>, who, help, quit" :bright-yellow)))
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
             (add-client player)
             (write-crlf stream (wrap "Welcome to the Endless Evening." :bright-cyan))
             (announce-to-room player (format nil "~a arrives." (wrap name :bright-green)) :include-self nil)
             (send-room-overview player)
             (loop
               (prompt stream)
               (let ((line (read-telnet-line stream)))
                 (when (null line)
                   (return))
                 (case (handle-command player line)
                   (:quit
                     (setf graceful t)
                     (write-crlf stream (wrap "May the stars guide your steps." :bright-yellow))
                     (return))))))))
      (when player
        (announce-to-room player (if graceful
                                      (format nil "~a fades into the night." (wrap (player-name player) :bright-blue))
                                      (format nil "~a disconnects." (wrap (player-name player) :bright-red)))))
      (when player
        (remove-client player))
      (ignore-errors (close stream))
      (ignore-errors (close socket)))))

(defun accept-loop (socket)
  (loop while *running* do
    (handler-case
        (let* ((client (socket-accept socket))
               (stream (socket-make-stream client :input t :output t :element-type 'character :external-format :latin-1 :buffering :line)))
          (make-thread (lambda () (client-loop client stream)) :name "mud-client"))
      (socket-error (err)
        (when *running*
          (log "Socket error: ~a" err))))))

(defun start (&key (port 4000))
  (when *running*
    (error "Server already running."))
  (initialize-world)
  (setf *running* t)
  (let ((socket (make-server-socket port)))
    (setf *listener* socket)
    (setf *listener-thread*
          (make-thread (lambda () (accept-loop socket)) :name "mud-acceptor"))
    (log "MUD listening on port ~a" port)
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
    (log "MUD server stopped.")))

(defun await ()
  (when *listener-thread*
    (join-thread *listener-thread*)))
