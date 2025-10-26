(in-package :cl-user)

;; Test for talk command compilation and execution
;; Created after debugging syntax error in talk command (2024-12-19)

(defun test-talk-command-compilation ()
  "Test that talk command compiles without syntax errors"
  (handler-case
      (progn
        (load "src/packages.lisp")
        (load "src/world.lisp")
        (load "src/player.lisp")
        (load "src/quest.lisp")
        (load "src/server/commands/core.lisp")
        (load "src/server/commands/player.lisp")
        (format t "✓ Talk command compilation successful~%")
        t)
    (error (e)
      (format t "✗ Talk command compilation failed: ~a~%" e)
      nil)))

(defun test-talk-command-syntax-validation ()
  "Test that talk command has proper parentheses balance"
  (handler-case
      (progn
        ;; Try to read the file and check for balanced parentheses
        (with-open-file (stream "src/server/commands/player.lisp")
          (let ((content (read-sequence (make-string (file-length stream)) stream)))
            (let ((open-count (count #\( content))
                  (close-count (count #\) content)))
              (if (= open-count close-count)
                  (progn
                    (format t "✓ Parentheses balanced: ~d open, ~d close~%" open-count close-count)
                    t)
                  (progn
                    (format t "✗ Parentheses unbalanced: ~d open, ~d close~%" open-count close-count)
                    nil)))))
    (error (e)
      (format t "✗ Syntax validation failed: ~a~%" e)
      nil)))

(defun test-talk-command-structure ()
  "Test that talk command has proper Lisp structure"
  (handler-case
      (progn
        ;; Load and try to evaluate the talk command definition
        (load "src/packages.lisp")
        (load "src/world.lisp")
        (load "src/player.lisp")
        (load "src/quest.lisp")
        (load "src/server/commands/core.lisp")
        (load "src/server/commands/player.lisp")
        
        ;; Check if the command is properly registered
        (let ((handler (gethash "talk" mud.server::*command-dispatch*)))
          (if handler
              (progn
                (format t "✓ Talk command properly registered~%")
                t)
              (progn
                (format t "✗ Talk command not registered~%")
                nil))))
    (error (e)
      (format t "✗ Structure validation failed: ~a~%" e)
      nil)))

(defun run-talk-command-tests ()
  "Run all talk command tests"
  (format t "~%=== Running Talk Command Tests ===~%")
  (let ((results (list
                  (test-talk-command-compilation)
                  (test-talk-command-syntax-validation)
                  (test-talk-command-structure))))
    (let ((passed (count t results))
          (total (length results)))
      (format t "~%=== Results: ~d/~d tests passed ===~%" passed total)
      (= passed total))))

;; Run tests if this file is executed directly
(when (member :run-tests *features*)
  (run-talk-command-tests))
