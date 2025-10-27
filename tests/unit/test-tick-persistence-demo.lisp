;; Demonstration of tick persistence feature
;; This shows how the global tick is now saved and restored

(in-package :cl-user)

;; Load the MUD system
(load "src/packages.lisp")
(load "src/ansi.lisp")
(load "src/player.lisp")
(load "src/inventory.lisp")
(load "src/world.lisp")
(load "src/server/core.lisp")
(load "src/server/runtime.lisp")

;; Demonstrate tick persistence
(let ((test-file "demo-tick-save.lisp"))
  
  (format t "=== Tick Persistence Demo ===~%")
  
  ;; Set up initial state
  (setf mud.world::*global-tick* 100)
  (format t "Initial global tick: ~d~%" mud.world::*global-tick*)
  
  ;; Simulate a save operation
  (format t "~%Saving game state...~%")
  (let ((snapshots '())  ; Empty player data for demo
        (current-tick (mud.world::get-global-tick)))
    (with-open-file (out test-file :direction :output :if-exists :supersede)
      (let ((*print-readably* t)
            (*print-escape* t)
            (*package* (find-package :cl)))
        (format out ";; Saved at ~a (demo)~%" (get-universal-time))
        (format out ";; Global tick: ~d~%" current-tick)
        (write snapshots :stream out :circle nil)
        (terpri out)))
    (format t "Saved with tick: ~d~%" current-tick))
  
  ;; Reset tick to simulate server restart
  (setf mud.world::*global-tick* 0)
  (format t "~%Reset global tick to: ~d~%" mud.world::*global-tick*)
  
  ;; Simulate loading the save file
  (format t "~%Loading game state...~%")
  (let ((saved-tick nil))
    (with-open-file (in test-file :direction :input)
      (let ((*read-eval* nil))
        ;; Read header comments to extract tick
        (loop for line = (read-line in nil nil)
              while line
              do (when (and (<= 15 (length line))
                            (let ((pos (search ";; Global tick:" line)))
                              (and pos (= pos 0))))
                   (let ((tick-str (string-trim " " (subseq line 15))))
                     (setf saved-tick (parse-integer tick-str :junk-allowed t))))
              until (or (null line) (not (and (<= 2 (length line))
                                              (let ((pos (search ";;" line)))
                                                (and pos (= pos 0)))))))
        ;; Read the actual data
        (let ((data (read in nil nil)))
          (when saved-tick
            (setf mud.world::*global-tick* saved-tick)
            (format t "Restored global tick to: ~d~%" saved-tick))))))
  
  ;; Verify the tick was restored
  (format t "~%Final global tick: ~d~%" mud.world::*global-tick*)
  
  ;; Clean up
  (ignore-errors (delete-file test-file))
  
  (format t "~%=== Demo Complete ===~%")
  (format t "The global tick is now persisted in save files!~%")
  (format t "This ensures game time continuity across server restarts.~%"))
