(in-package :mud.server)


(defvar *listener*)
(defvar *listener-thread*)
(defvar *clients*)
(defvar *clients-lock*)
(defvar *running*)


(defparameter *state-file-path* nil)
(defparameter *persistence-interval* 60)
(defparameter *persistence-thread* nil)
(defparameter *persistence-semaphore* nil)
(defparameter *persistence-running* nil)
(defparameter *persistence-lock* (make-mutex :name "persistence-lock"))

;; Mob movement system - use global tick interval
(defparameter *mob-movement-thread* nil)
(defparameter *mob-movement-running* nil)

(defun resolve-state-file-path ()
  (let ((override (sb-ext:posix-getenv "MUD_STATE_FILE")))
    (handler-case
        (if (and override (> (length override) 0))
            (pathname override)
            (merge-pathnames #P"data/save-state.lisp" *default-pathname-defaults*))
      (error (err)
        (declare (ignore err))
        (merge-pathnames #P"data/save-state.lisp" *default-pathname-defaults*)))))

(defun state-file-path ()
  (or *state-file-path*
      (setf *state-file-path* (resolve-state-file-path))))

(defun save-game-state (&key (reason :periodic))
  (let ((path (state-file-path)))
    (with-mutex (*persistence-lock*)
      (handler-case
          (let ((snapshots (collect-player-snapshots))
                (current-tick (mud.world::get-global-tick)))
            (ensure-directories-exist path)
            (with-open-file (out path :direction :output :if-exists :supersede
                                  :if-does-not-exist :create)
              (let ((*print-readably* t)
                    (*print-escape* t)
                    (*package* (find-package :cl)))
                (format out ";; Saved at ~a (~(~a~))~%" (get-universal-time) reason)
                (format out ";; Global tick: ~d~%" current-tick)
                (write snapshots :stream out :circle nil)
                (terpri out)
                (finish-output out)))
            (let ((count (length snapshots)))
              (when (not (eq reason :periodic))
                (server-log "Saved ~d player~:p to ~a (~(~a~)) at tick ~d" count path reason current-tick))
              count))
        (error (err)
          (server-log "Failed to save game state (~(~a~)): ~a" reason err)
          nil)))))

(eval-when (:load-toplevel)
  (format t "Loading runtime persistence helpers...~%")
  (finish-output))

(defun load-game-state ()
  (let* ((path (state-file-path))
         (default-room (room-id (starting-room)))
         (valid-room-p #'find-room))
    (if (and path (probe-file path))
        (handler-case
            (with-open-file (in path :direction :input)
              (let ((*read-eval* nil)
                    (saved-tick nil))
                ;; Read header comments to extract tick
                (loop for line = (read-line in nil nil)
                      while line
                      do (when (and (<= 15 (length line))
                                    (let ((pos (search ";; Global tick:" line)))
                                      (and pos (= pos 0)))
                                    (not (string= line "")))
                           (let ((tick-str (string-trim " " (subseq line 15))))
                             (setf saved-tick (parse-integer tick-str :junk-allowed t))))
                      until (or (null line) (not (and (<= 2 (length line))
                                                      (let ((pos (search ";;" line)))
                                                        (and pos (= pos 0)))))))
                ;; Restore global tick if found (regardless of player data)
                (when saved-tick
                  (setf mud.world::*global-tick* saved-tick)
                  (server-log "Restored global tick to ~d" saved-tick))
                ;; Read the actual data
                (let ((data (read in nil nil)))
                  (if (listp data)
                      (let ((restored (restore-player-snapshots data
                                                               :default-room default-room
                                                               :valid-room-p valid-room-p)))
                        (when (> restored 0)
                          (server-log "Loaded ~d player~:p from ~a" restored path))
                        restored)
                      0))))
          (error (err)
            (server-log "Failed to load game state from ~a: ~a" path err)
            0))
        0)))

(defun ensure-persistence-semaphore ()
  (unless *persistence-semaphore*
    (setf *persistence-semaphore* (make-semaphore :count 0 :name "persistence-wakeup"))))

(defun start-persistence-loop ()
  (ensure-persistence-semaphore)
  (unless *persistence-thread*
    (setf *persistence-running* t)
    (setf *persistence-thread*
          (make-thread #'persistence-loop :name "mud-persistence"))))

(defun stop-persistence-loop ()
  (when *persistence-thread*
    (setf *persistence-running* nil)
    (when *persistence-semaphore*
      (signal-semaphore *persistence-semaphore*))
    (handler-case
        (join-thread *persistence-thread*)
      (error (err)
        (server-log "Failed to join persistence thread: ~a" err)))
    (setf *persistence-thread* nil))
  (setf *persistence-running* nil)
  (setf *persistence-semaphore* nil))

(defun persistence-loop ()
  (loop
    (unless *persistence-running*
      (return))
    (let ((signaled (and *persistence-semaphore*
                         (wait-on-semaphore *persistence-semaphore* :timeout *persistence-interval*))))
      (unless *persistence-running*
        (return))
      (when (and *persistence-running* (not signaled))
        (save-game-state :reason :periodic)))))

(defun announce-to-room-by-id (room-id message &key (exclude nil))
  "Announce a message to all players in a specific room"
  (with-mutex (*clients-lock*)
    (dolist (client *clients*)
      (when (and (eq (player-room client) room-id)
                 (not (member client exclude)))
        (ignore-errors (write-crlf (player-stream client) message))))))

(defun announce-mob-movement (mob old-room-id new-room-id)
  "Announce mob movement to players in affected rooms"
  (let ((mob-name (mud.mob::mob-name mob))
        (old-room (mud.world::find-room old-room-id))
        (new-room (mud.world::find-room new-room-id)))
    (when old-room
      (announce-to-room-by-id old-room-id
        (format nil "~a wanders away." (mud.ansi::wrap mob-name :bright-red))
        :exclude nil))
    (when new-room
      (announce-to-room-by-id new-room-id
        (format nil "~a wanders in." (mud.ansi::wrap mob-name :bright-red))
        :exclude nil))))

(defun mob-movement-loop ()
  (loop
    (unless *mob-movement-running*
      (return))
    (sleep mud.constants::*tick-interval*)
    (unless *mob-movement-running*
      (return))
    (handler-case
        (progn
          ;; Advance global tick and world time
          (mud.world::advance-global-tick)
          ;; Process mob movements
          (let ((movements (mud.mob::process-all-mob-movements)))
            (when movements
              (dolist (movement movements)
                (destructuring-bind (mob old-room new-room) movement
                  (announce-mob-movement mob old-room new-room)))))
          ;; Process mob combat
          (mud.mob::process-all-mob-combat)
          ;; Process tick events
          (mud.events::process-tick-events))
      (error (err)
        (server-log "Error in mob movement loop: ~a" err)))))

(defun start-mob-movement-loop ()
  (unless *mob-movement-thread*
    (setf *mob-movement-running* t)
    (setf *mob-movement-thread*
          (make-thread #'mob-movement-loop :name "mud-mob-movement"))))

(defun stop-mob-movement-loop ()
  (when *mob-movement-thread*
    (setf *mob-movement-running* nil)
    (handler-case
        (join-thread *mob-movement-thread*)
      (error (err)
        (server-log "Failed to join mob movement thread: ~a" err)))
    (setf *mob-movement-thread* nil))
  (setf *mob-movement-running* nil))


(defun client-loop (socket stream)
  (let ((player nil) (graceful nil))
    (unwind-protect
        (let ((name (ask-for-name stream)))
          (unless name (return-from client-loop))
          (let* ((start (starting-room)) (room-id (room-id start)))
            (multiple-value-bind (reclaimed new-player-p)
                (get-or-create-player :name name :room room-id
                                      :stream stream :socket socket)
              (setf player reclaimed)
              (when new-player-p
                (dotimes (i 3)
                  (add-to-inventory player (create-item "mana-potion"))))
              (add-client player)
              (write-crlf stream
               (if new-player-p
                   (wrap "Welcome to the Endless Evening." :bright-cyan)
                   (wrap "Welcome back to the Endless Evening." :bright-cyan)))
              (let ((arrival-text
                      (if new-player-p
                          (format nil "~a arrives." (wrap name :bright-green))
                          (format nil "~a returns." (wrap name :bright-green)))))
                (announce-to-room player arrival-text :include-self nil))
              (send-room-overview player)
              (let ((last-command nil))
                (loop (prompt stream)
                      (let ((line (read-telnet-line stream)))
                        (when (null line) (return))
                        (when (and (string= line ".") last-command)
                          (setf line last-command))
                        (unless (string= line ".") (setf last-command line))
                        (case (handle-command player line)
                          (:quit
                           (setf graceful t)
                           (write-crlf stream
                            (wrap "May the stars guide your steps."
                             :bright-yellow))
                           (return))))))))))
    (when player
      (announce-to-room player
       (if graceful
           (format nil "~a fades into the night."
                   (wrap (player-name player) :bright-blue))
           (format nil "~a disconnects."
                   (wrap (player-name player) :bright-red)))))
    (when player
      (remove-client player)
      (detach-player player))
    (ignore-errors (close stream))
    (ignore-errors (close socket))))


(defun accept-loop (socket)
  (loop while *running*
        do (handler-case
            (let* ((client (socket-accept socket))
                   (stream
                    (socket-make-stream client :input t :output t :element-type
                     'character :external-format :latin-1 :buffering :line)))
              (make-thread #'client-loop :name "mud-client" :arguments
               (list client stream)))
            (socket-error (err)
             (when *running* (server-log "Socket error: ~a" err))))))


(defun start (&key (port 4000))
  (when *running* (error "Server already running."))
  (initialize-world)
  (initialize-merchants)
  (initialize-quests)
  (initialize-mobs)
  (load-game-state)
  (setf *running* t)
  (start-persistence-loop)
  (start-mob-movement-loop)
  (let ((socket (make-server-socket port)))
    (setf *listener* socket)
    (setf *listener-thread*
            (make-thread (lambda () (accept-loop socket)) :name
             "mud-acceptor"))
    (server-log "MUD listening on port ~a" port)
    *listener*))


(defun stop ()
  (when (or *running* *listener* *listener-thread* *persistence-thread*)
    (let ((was-running *running*))
      (setf *running* nil)
      (stop-persistence-loop)
      (stop-mob-movement-loop)
      (when *listener*
        (ignore-errors (close *listener*))
        (setf *listener* nil))
      (when *listener-thread*
        (join-thread *listener-thread*)
        (setf *listener-thread* nil))
      (with-mutex (*clients-lock*)
       (dolist (client *clients*)
         (ignore-errors (close (player-stream client)))
         (ignore-errors (close (player-socket client)))
         (detach-player client))
       (setf *clients* 'nil))
      (save-game-state :reason (if was-running :shutdown :manual))
      (server-log "MUD server stopped."))))


(defun await () (when *listener-thread* (join-thread *listener-thread*)))
