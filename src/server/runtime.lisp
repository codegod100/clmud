(in-package :mud.server)


(defun client-loop (socket stream)
  (let ((player nil) (graceful nil))
    (unwind-protect
        (let ((name (ask-for-name stream)))
          (unless name (return-from client-loop))
          (let* ((start (starting-room)) (room-id (room-id start)))
            (setf player
                    (make-player :name name :room room-id :stream stream
                     :socket socket))
            (dotimes (i 3)
              (add-to-inventory player (create-item "mana-potion")))
            (add-client player)
            (write-crlf stream
             (wrap "Welcome to the Endless Evening." :bright-cyan))
            (announce-to-room player
             (format nil "~a arrives." (wrap name :bright-green)) :include-self
             nil)
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
                         (return)))))))))
    (when player
      (announce-to-room player
       (if graceful
           (format nil "~a fades into the night."
                   (wrap (player-name player) :bright-blue))
           (format nil "~a disconnects."
                   (wrap (player-name player) :bright-red)))))
    (when player (remove-client player))
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
  (setf *running* t)
  (let ((socket (make-server-socket port)))
    (setf *listener* socket)
    (setf *listener-thread*
            (make-thread (lambda () (accept-loop socket)) :name
             "mud-acceptor"))
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
     (setf *clients* 'nil))
    (server-log "MUD server stopped.")))


(defun await () (when *listener-thread* (join-thread *listener-thread*)))
