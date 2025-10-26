#!/usr/bin/env sbcl --script

;; Main test runner for the CLMUD project
;; This script runs all tests in the proper order and provides a unified interface

(require :sb-bsd-sockets)
(require :asdf)

;; Load the MUD system dependencies
(load "../src/packages.lisp")
(load "../src/ansi.lisp")
(load "../src/player.lisp")
(load "../src/inventory.lisp")
(load "../src/merchant.lisp")
(load "../src/world.lisp")
(load "../src/mob.lisp")
(load "../src/combat.lisp")
(load "../src/quest.lisp")
(load "../src/server/core.lisp")

;; Initialize the world
(mud.world:initialize-world)

(defparameter *test-results* (make-hash-table :test #'equal))
(defparameter *total-tests* 0)
(defparameter *passed-tests* 0)
(defparameter *failed-tests* 0)

(defun run-test-file (file-path test-name)
  "Run a single test file and record results"
  (format t "~%~%=== Running ~a ===~%" test-name)
  (incf *total-tests*)
  
  (handler-case
      (progn
        (load file-path)
        (setf (gethash test-name *test-results*) :passed)
        (incf *passed-tests*)
        (format t "✓ ~a PASSED~%" test-name))
      (error (err)
        (setf (gethash test-name *test-results*) :failed)
        (incf *failed-tests*)
        (format t "✗ ~a FAILED: ~a~%" test-name err))))

(defun run-test-suite (suite-name test-files)
  "Run a suite of related tests"
  (format t "~%~%========================================~%")
  (format t "Running ~a Test Suite~%" suite-name)
  (format t "========================================~%")
  
  (dolist (test-file test-files)
    (run-test-file (car test-file) (cdr test-file))))

(defun print-summary ()
  "Print test execution summary"
  (format t "~%~%========================================~%")
  (format t "TEST EXECUTION SUMMARY~%")
  (format t "========================================~%")
  (format t "Total tests: ~a~%" *total-tests*)
  (format t "Passed: ~a~%" *passed-tests*)
  (format t "Failed: ~a~%" *failed-tests*)
  (format t "Success rate: ~,1f%~%" 
          (if (> *total-tests* 0) 
              (* 100.0 (/ *passed-tests* *total-tests*))
              0))
  
  (when (> *failed-tests* 0)
    (format t "~%Failed tests:~%")
    (maphash (lambda (test-name result)
               (when (eq result :failed)
                 (format t "  - ~a~%" test-name)))
             *test-results*))
  
  (format t "~%========================================~%"))

;; Define test suites
(defparameter *unit-tests*
  '(("unit/test-command-registration.lisp" . "Command Registration")
    ("unit/test-commands-load.lisp" . "Commands Load")
    ("unit/test-macro-expansion.lisp" . "Macro Expansion")))

(defparameter *command-tests*
  '(("commands/test-get-command-compilation.lisp" . "Get Command Compilation")
    ("commands/test-get-command.lisp" . "Get Command Basic")
    ("commands/test-get-step-by-step.lisp" . "Get Command Step-by-Step")
    ("commands/test-minimal-get.lisp" . "Minimal Get Command")))

(defparameter *integration-tests*
  '(("integration/test-get-with-add-inventory.lisp" . "Get with Add Inventory")
    ("integration/test-get-with-announce.lisp" . "Get with Announce")
    ("integration/test-get-with-loot.lisp" . "Get with Loot")
    ("integration/test-get-with-quest-rewards.lisp" . "Get with Quest Rewards")
    ("integration/test-get-with-remove-item.lisp" . "Get with Remove Item")
    ("integration/test-get-with-write-crlf.lisp" . "Get with Write CRLF")))

;; Run all test suites
(format t "Starting CLMUD Test Suite Execution~%")
(format t "====================================~%")

(run-test-suite "Unit Tests" *unit-tests*)
(run-test-suite "Command Tests" *command-tests*)
(run-test-suite "Integration Tests" *integration-tests*)

;; Run the FiveAM test suite if available
(format t "~%~%========================================~%")
(format t "Running FiveAM Test Suite~%")
(format t "========================================~%")
(handler-case
    (progn
      (load "test-runner.lisp")
      (format t "✓ FiveAM tests completed~%"))
    (error (err)
      (format t "✗ FiveAM tests failed: ~a~%" err)
      (incf *failed-tests*)))

(print-summary)

;; Exit with appropriate code
(if (> *failed-tests* 0)
    (sb-ext:exit :code 1)
    (sb-ext:exit :code 0))
