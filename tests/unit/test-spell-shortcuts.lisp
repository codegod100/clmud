(in-package :mud.combat)

;;; Test spell shortcut functionality
;;; Tests that partial spell names work like target shortcuts

;; Export the test function
(export 'run-spell-shortcut-tests)

(defun test-shortcut (input expected-spell-name)
  "Test a single spell shortcut"
  (let ((result (find-spell input)))
    (if expected-spell-name
        (and result (string-equal (spell-name result) expected-spell-name))
        (null result))))

(defun test-spell-shortcuts ()
  "Test that spell shortcuts work correctly"
  (let ((test-results '()))
    
    ;; Test basic shortcuts
    (push (test-shortcut "light" "lightning") test-results)
    (push (test-shortcut "fire" "fireball") test-results)
    (push (test-shortcut "ice" "ice-shard") test-results)
    (push (test-shortcut "heal" "heal") test-results)
    (push (test-shortcut "drain" "drain") test-results)
    
    ;; Test that full names still work
    (push (test-shortcut "lightning" "lightning") test-results)
    (push (test-shortcut "fireball" "fireball") test-results)
    (push (test-shortcut "ice-shard" "ice-shard") test-results)
    
    ;; Test case insensitivity
    (push (test-shortcut "LIGHT" "lightning") test-results)
    (push (test-shortcut "Fire" "fireball") test-results)
    (push (test-shortcut "ICE" "ice-shard") test-results)
    
    ;; Test non-existent spells
    (push (test-shortcut "nonexistent" nil) test-results)
    (push (test-shortcut "xyz" nil) test-results)
    
    ;; Report results
    (let ((passed (count t test-results))
          (total (length test-results)))
      (format t "Spell shortcut tests: ~a/~a passed~%" passed total)
      (when (< passed total)
        (format t "Failed tests:~%")
        (loop for result in test-results
              for i from 0
              when (not result)
              do (format t "  Test ~a failed~%" i)))
      (= passed total))))

;;; Run the tests
(defun run-spell-shortcut-tests ()
  "Run all spell shortcut tests"
  (format t "Running spell shortcut tests...~%")
  (test-spell-shortcuts))

;;; Make it easy to run from REPL
(format t "Spell shortcut tests loaded. Run (mud.combat:run-spell-shortcut-tests) to test.~%")
