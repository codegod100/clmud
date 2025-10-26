;; Simple test for say and chat commands
;; Tests that the command definitions exist in the source files

(in-package :cl-user)

(defun test-say-command-exists ()
  "Test that the say command is defined in the source"
  (let ((say-file "src/server/commands/player.lisp"))
    (with-open-file (stream say-file :direction :input)
      (let ((content (make-string (file-length stream))))
        (read-sequence content stream)
        (if (search "define-command ((\"say\")" content)
            (progn
              (format t "✓ Say command found in source~%")
              t)
            (progn
              (format t "✗ Say command NOT found in source~%")
              nil))))))

(defun test-chat-command-exists ()
  "Test that the chat command is defined in the source"
  (let ((chat-file "src/server/commands/player.lisp"))
    (with-open-file (stream chat-file :direction :input)
      (let ((content (make-string (file-length stream))))
        (read-sequence content stream)
        (if (search "define-command ((\"chat\")" content)
            (progn
              (format t "✓ Chat command found in source~%")
              t)
            (progn
              (format t "✗ Chat command NOT found in source~%")
              nil))))))

(defun test-handle-say-exists ()
  "Test that the handle-say function is defined in the source"
  (let ((core-file "src/server/core.lisp"))
    (with-open-file (stream core-file :direction :input)
      (let ((content (make-string (file-length stream))))
        (read-sequence content stream)
        (if (search "defun handle-say" content)
            (progn
              (format t "✓ handle-say function found in source~%")
              t)
            (progn
              (format t "✗ handle-say function NOT found in source~%")
              nil))))))

(defun test-handle-chat-exists ()
  "Test that the handle-chat function is defined in the source"
  (let ((core-file "src/server/core.lisp"))
    (with-open-file (stream core-file :direction :input)
      (let ((content (make-string (file-length stream))))
        (read-sequence content stream)
        (if (search "defun handle-chat" content)
            (progn
              (format t "✓ handle-chat function found in source~%")
              t)
            (progn
              (format t "✗ handle-chat function NOT found in source~%")
              nil))))))

(defun test-help-command-updated ()
  "Test that the help command mentions both say and chat"
  (let ((help-file "src/server/commands/player.lisp"))
    (with-open-file (stream help-file :direction :input)
      (let ((content (make-string (file-length stream))))
        (read-sequence content stream)
        (if (and (search "say <text>" content) (search "chat <text>" content))
            (progn
              (format t "✓ Help command mentions both say and chat~%")
              t)
            (progn
              (format t "✗ Help command does not mention both say and chat~%")
              nil))))))

(defun run-say-chat-simple-tests ()
  "Run simple tests for say and chat commands"
  (format t "~%=== Testing Say and Chat Commands (Source Analysis) ===~%")
  
  (let ((results '()))
    (push (test-say-command-exists) results)
    (push (test-chat-command-exists) results)
    (push (test-handle-say-exists) results)
    (push (test-handle-chat-exists) results)
    (push (test-help-command-updated) results)
    
    (let ((passed (count t results))
          (total (length results)))
      (format t "~%=== Test Results ===~%")
      (format t "Passed: ~d/~d~%" passed total)
      (if (= passed total)
          (format t "✓ All tests passed!~%")
          (format t "✗ Some tests failed~%"))
      (= passed total))))

;; Run tests
(run-say-chat-simple-tests)


