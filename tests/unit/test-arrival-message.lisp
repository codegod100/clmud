;; Test for arrival message showing source room

(in-package :cl-user)

(defun test-arrival-message-format ()
  "Test that the arrival message includes source room information"
  (let ((core-file "src/server/core.lisp"))
    (with-open-file (stream core-file :direction :input)
      (let ((content (make-string (file-length stream))))
        (read-sequence content stream)
        (if (search "arrives from" content)
            (progn
              (format t "✓ Arrival message includes source room~%")
              t)
            (progn
              (format t "✗ Arrival message does not include source room~%")
              nil))))))

(defun test-arrival-message-color-coding ()
  "Test that the arrival message has proper color coding"
  (let ((core-file "src/server/core.lisp"))
    (with-open-file (stream core-file :direction :input)
      (let ((content (make-string (file-length stream))))
        (read-sequence content stream)
        (if (and (search ":bright-green" content) (search ":bright-cyan" content))
            (progn
              (format t "✓ Arrival message has proper color coding~%")
              t)
            (progn
              (format t "✗ Arrival message missing color coding~%")
              nil))))))

(defun run-arrival-message-tests ()
  "Run tests for arrival message functionality"
  (format t "~%=== Testing Arrival Message Updates ===~%")
  
  (let ((results '()))
    (push (test-arrival-message-format) results)
    (push (test-arrival-message-color-coding) results)
    
    (let ((passed (count t results))
          (total (length results)))
      (format t "~%=== Test Results ===~%")
      (format t "Passed: ~d/~d~%" passed total)
      (if (= passed total)
          (format t "✓ All tests passed!~%")
          (format t "✗ Some tests failed~%"))
      (= passed total))))

;; Run tests
(run-arrival-message-tests)
