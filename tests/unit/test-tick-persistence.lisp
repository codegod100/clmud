;; Test tick persistence in save/load system
;; This test verifies that the global tick is properly saved and restored

(in-package :cl-user)

(defun test-tick-persistence ()
  "Test that global tick is properly saved and restored"
  (let ((test-file "test-tick-save.lisp")
        (original-tick nil)
        (restored-tick nil))
    
    ;; Load the necessary packages in correct order
    (load "src/packages.lisp")
    (load "src/ansi.lisp")
    (load "src/player.lisp")
    (load "src/inventory.lisp")
    (load "src/world.lisp")
    (load "src/server/core.lisp")
    (load "src/server/runtime.lisp")
    
    ;; Set up a test tick value
    (setf mud.world::*global-tick* 42)
    (setf original-tick mud.world::*global-tick*)
    
    ;; Create a mock save file with tick information
    (with-open-file (out test-file :direction :output :if-exists :supersede)
      (let ((*print-readably* t)
            (*print-escape* t)
            (*package* (find-package :cl)))
        (format out ";; Saved at ~a (test)~%" (get-universal-time))
        (format out ";; Global tick: ~d~%" original-tick)
        (write '() :stream out :circle nil)  ; Empty player data
        (terpri out)))
    
    ;; Reset tick to 0
    (setf mud.world::*global-tick* 0)
    
    ;; Test the load function
    (let ((path (mud.server::resolve-state-file-path)))
      (setf mud.server::*state-file-path* test-file)
      (let ((restored-count (mud.server::load-game-state)))
        (setf restored-tick mud.world::*global-tick*)
        
        ;; Clean up test file
        (ignore-errors (delete-file test-file))
        
        ;; Verify results
        (if (and (= restored-tick original-tick)
                 (= restored-count 0))
            (progn
              (format t "✓ Tick persistence test PASSED~%")
              (format t "  Original tick: ~d~%" original-tick)
              (format t "  Restored tick: ~d~%" restored-tick)
              t)
            (progn
              (format t "✗ Tick persistence test FAILED~%")
              (format t "  Original tick: ~d~%" original-tick)
              (format t "  Restored tick: ~d~%" restored-tick)
              (format t "  Restored count: ~d~%" restored-count)
              nil))))))

;; Run the test
(test-tick-persistence)
