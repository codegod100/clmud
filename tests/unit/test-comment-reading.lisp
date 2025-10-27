;; Test that the load function correctly reads tick from comments

;; Create a test save file that mimics the real format
(let ((test-file "test-comment-save.lisp")
      (original-tick 123))
  
  ;; Create a save file with the exact format used by save-game-state
  (with-open-file (out test-file :direction :output :if-exists :supersede)
    (let ((*print-readably* t)
          (*print-escape* t)
          (*package* (find-package :cl)))
      (format out ";; Saved at ~a (test)~%" (get-universal-time))
      (format out ";; Global tick: ~d~%" original-tick)
      (write '() :stream out :circle nil)  ; Empty player data
      (terpri out)))
  
  ;; Test the exact parsing logic from load-game-state
  (let ((saved-tick nil))
    (with-open-file (in test-file :direction :input)
      (let ((*read-eval* nil))
        ;; This is the exact logic from load-game-state
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
                                                (and pos (= pos 0))))))))
    
    ;; Clean up
    (ignore-errors (delete-file test-file))
    
    ;; Report results
    (format t "Original tick: ~d~%" original-tick)
    (format t "Parsed tick: ~d~%" saved-tick)
    (if (= saved-tick original-tick)
        (format t "✓ Comment reading test PASSED~%")
        (format t "✗ Comment reading test FAILED~%"))))
