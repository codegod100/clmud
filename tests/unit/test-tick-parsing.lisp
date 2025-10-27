;; Test tick parsing logic without package dependencies

;; Test that we can parse tick information from save file format
(let ((test-file "test-tick-save.lisp")
      (original-tick 42))
  
  ;; Create a test save file with tick information
  (with-open-file (out test-file :direction :output :if-exists :supersede)
    (let ((*print-readably* t)
          (*print-escape* t)
          (*package* (find-package :cl)))
      (format out ";; Saved at ~a (test)~%" (get-universal-time))
      (format out ";; Global tick: ~d~%" original-tick)
      (write '() :stream out :circle nil)  ; Empty player data
      (terpri out)))
  
  ;; Test parsing the tick from file
  (let ((saved-tick nil))
    (with-open-file (in test-file :direction :input)
      (loop for line = (read-line in nil nil)
            while line
            do (when (and (<= 15 (length line))
                          (let ((pos (search ";; Global tick:" line)))
                            (and pos (= pos 0))))
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
        (format t "✓ Tick parsing test PASSED~%")
        (format t "✗ Tick parsing test FAILED~%"))))
