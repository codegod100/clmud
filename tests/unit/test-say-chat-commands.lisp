;; Test file for say and chat commands
;; Tests both local (say) and global (chat) communication commands

;; Simple test that verifies the command registration without full server load
(in-package :cl-user)

(defun test-say-command-registration ()
  "Test that the say command is properly registered"
  (let ((handler (gethash "say" mud.server:*command-dispatch*)))
    (if handler
        (progn
          (format t "✓ Say command registered successfully~%")
          t)
        (progn
          (format t "✗ Say command NOT registered~%")
          nil))))

(defun test-chat-command-registration ()
  "Test that the chat command is properly registered"
  (let ((handler (gethash "chat" mud.server:*command-dispatch*)))
    (if handler
        (progn
          (format t "✓ Chat command registered successfully~%")
          t)
        (progn
          (format t "✗ Chat command NOT registered~%")
          nil))))

(defun test-say-command-empty ()
  "Test say command with empty input"
  (let* ((mock-stream (make-string-output-stream))
         (player (mud.player:make-player :name "TestPlayer" :room 1 :stream mock-stream :socket nil)))
    (mud.server:handle-say player "")
    (let ((output (get-output-stream-string mock-stream)))
      (if (search "Say what?" output)
          (progn
            (format t "✓ Say command handles empty input correctly~%")
            t)
          (progn
            (format t "✗ Say command failed to handle empty input~%")
            nil)))))

(defun test-chat-command-empty ()
  "Test chat command with empty input"
  (let* ((mock-stream (make-string-output-stream))
         (player (mud.player:make-player :name "TestPlayer" :room 1 :stream mock-stream :socket nil)))
    (mud.server:handle-chat player "")
    (let ((output (get-output-stream-string mock-stream)))
      (if (search "Chat what?" output)
          (progn
            (format t "✓ Chat command handles empty input correctly~%")
            t)
          (progn
            (format t "✗ Chat command failed to handle empty input~%")
            nil)))))

(defun test-say-command-whitespace ()
  "Test say command with whitespace-only input"
  (let* ((mock-stream (make-string-output-stream))
         (player (mud.player:make-player :name "TestPlayer" :room 1 :stream mock-stream :socket nil)))
    (mud.server:handle-say player "   ")
    (let ((output (get-output-stream-string mock-stream)))
      (if (search "Say what?" output)
          (progn
            (format t "✓ Say command handles whitespace input correctly~%")
            t)
          (progn
            (format t "✗ Say command failed to handle whitespace input~%")
            nil)))))

(defun test-chat-command-whitespace ()
  "Test chat command with whitespace-only input"
  (let* ((mock-stream (make-string-output-stream))
         (player (mud.player:make-player :name "TestPlayer" :room 1 :stream mock-stream :socket nil)))
    (mud.server:handle-chat player "   ")
    (let ((output (get-output-stream-string mock-stream)))
      (if (search "Chat what?" output)
          (progn
            (format t "✓ Chat command handles whitespace input correctly~%")
            t)
          (progn
            (format t "✗ Chat command failed to handle whitespace input~%")
            nil)))))

(defun test-say-command-valid-message ()
  "Test say command with valid message"
  (let* ((mock-stream (make-string-output-stream))
         (player (mud.player:make-player :name "TestPlayer" :room 1 :stream mock-stream :socket nil)))
    (mud.server:handle-say player "Hello world!")
    (let ((output (get-output-stream-string mock-stream)))
      (if (and (search "TestPlayer" output) (search "says:" output) (search "Hello world!" output))
          (progn
            (format t "✓ Say command handles valid message correctly~%")
            t)
          (progn
            (format t "✗ Say command failed to handle valid message~%")
            nil)))))

(defun test-chat-command-valid-message ()
  "Test chat command with valid message"
  (let* ((mock-stream (make-string-output-stream))
         (player (mud.player:make-player :name "TestPlayer" :room 1 :stream mock-stream :socket nil)))
    (mud.server:handle-chat player "Hello everyone!")
    (let ((output (get-output-stream-string mock-stream)))
      (if (and (search "TestPlayer" output) (search "chats:" output) (search "Hello everyone!" output))
          (progn
            (format t "✓ Chat command handles valid message correctly~%")
            t)
          (progn
            (format t "✗ Chat command failed to handle valid message~%")
            nil)))))

(defun run-say-chat-tests ()
  "Run all say and chat command tests"
  (format t "~%=== Testing Say and Chat Commands ===~%")
  
  (let ((results '()))
    (push (test-say-command-registration) results)
    (push (test-chat-command-registration) results)
    (push (test-say-command-empty) results)
    (push (test-chat-command-empty) results)
    (push (test-say-command-whitespace) results)
    (push (test-chat-command-whitespace) results)
    (push (test-say-command-valid-message) results)
    (push (test-chat-command-valid-message) results)
    
    (let ((passed (count t results))
          (total (length results)))
      (format t "~%=== Test Results ===~%")
      (format t "Passed: ~d/~d~%" passed total)
      (if (= passed total)
          (format t "✓ All tests passed!~%")
          (format t "✗ Some tests failed~%"))
      (= passed total))))

;; Run tests if this file is executed directly
(when (member :test-runner *features*)
  (run-say-chat-tests))
