(in-package :mud.game-state)

(defstruct game-state
  (timestamp 0 :type integer)
  (global-tick 0 :type integer)
  (players nil :type list))

(declaim (ftype (function (&rest t &key (:path t) &allow-other-keys) t) save-game-state))

(defun save-game-state (&rest args &key ((:path key-path) nil key-supplied-p) &allow-other-keys)
  "Save the current game state to a file"
  (let* ((positional-path (and args (not (keywordp (first args))) (first args)))
         (raw-path (cond
                     (key-supplied-p key-path)
                     (positional-path positional-path)
                     (t (error "SAVE-GAME-STATE requires a path argument (either positional or via :PATH)."))))
         (target (cond
                   ((pathnamep raw-path) raw-path)
                   ((stringp raw-path) (pathname raw-path))
                   (t (error "SAVE-GAME-STATE expected a pathname or string for the path argument, got ~S."
                             raw-path))))
         (state (make-game-state
                 :timestamp (get-universal-time)
                 :global-tick mud.world::*global-tick*
                 :players (mud.player::collect-player-snapshots))))
    (with-open-file (out target :direction :output :if-exists :supersede)
      (let ((*print-pretty* t)
            (*print-readably* t))
        (print state out)))
    (length (game-state-players state))))

(defun load-game-state (path)
  "Load game state from a file"
  (if (probe-file path)
      (with-open-file (in path :direction :input)
        (let ((*read-eval* nil))
          (let ((state (read in nil nil)))
            (if state
                (progn
                  (setf mud.world::*global-tick* (game-state-global-tick state))
                  (mud.player::restore-player-snapshots (game-state-players state)
                                                        :default-room 'village-square
                                                        :valid-room-p #'mud.world::find-room))
                0))))
      0))
