#!/usr/bin/env sbcl --script

;; Test script for the repair spell functionality
;; This tests the repair spell logic without requiring a full server setup

(load "src/packages.lisp")
(load "src/world.lisp")
(load "src/player.lisp")
(load "src/inventory.lisp")
(load "src/combat.lisp")

(defun test-repair-spell-basic ()
  "Test basic repair spell functionality"
  (format t "Testing repair spell basic functionality...~%")
  
  ;; Test that repair spell is defined
  (let ((repair-spell (mud.combat:find-spell "repair")))
    (if repair-spell
        (progn
          (format t "✓ Repair spell found: ~a~%" (mud.combat:spell-name repair-spell))
          (format t "  Cost: ~d mana~%" (mud.combat:spell-cost repair-spell))
          (format t "  Description: ~a~%" (mud.combat:spell-description repair-spell)))
        (format t "✗ Repair spell not found!~%")))
  
  ;; Test spell list includes repair
  (let ((spell-names (mapcar #'mud.combat:spell-name mud.combat:*spells*)))
    (if (member "repair" spell-names :test #'string-equal)
        (format t "✓ Repair spell is in the spells list~%")
        (format t "✗ Repair spell not in spells list!~%")))
  
  (format t "~%"))

(defun test-repair-spell-logic ()
  "Test repair spell logic with mock data"
  (format t "Testing repair spell logic...~%")
  
  ;; Create a mock player with a vehicle
  (let* ((player (mud.player:make-player :name "TestPlayer"))
         (vehicle-item (mud.inventory:make-item :name "test-car" :type :vehicle)))
    
    ;; Set high mana for testing
    (setf (mud.player:player-mana player) 100)
    
    ;; Set up the player with a vehicle
    (setf (mud.player:player-vehicle player) vehicle-item)
    
    ;; Define the vehicle template
    (mud.world:define-vehicle "test-car" :ground "A test car" :armor 5)
    
    ;; Get the vehicle template and damage it
    (let ((vehicle-template (mud.world:find-vehicle "test-car")))
      (setf (mud.world:vehicle-armor vehicle-template) 2)
      
      (format t "  Vehicle armor before repair: ~d/~d~%" 
              (mud.world:vehicle-armor vehicle-template)
              (mud.world:vehicle-max-armor vehicle-template))
      
      ;; Test casting repair spell
      (multiple-value-bind (success message death-occurred)
          (mud.combat:cast-spell player player "repair")
        (if success
            (progn
              (format t "✓ Repair spell cast successfully~%")
              (format t "  Message: ~a~%" message)
              (format t "  Vehicle armor after repair: ~d/~d~%" 
                      (mud.world:vehicle-armor vehicle-template)
                      (mud.world:vehicle-max-armor vehicle-template))
              (if (= (mud.world:vehicle-armor vehicle-template) 
                     (mud.world:vehicle-max-armor vehicle-template))
                  (format t "✓ Vehicle fully repaired!~%")
                  (format t "✗ Vehicle not fully repaired!~%")))
            (progn
              (format t "✗ Repair spell failed: ~a~%" message))))
      
      ;; Test repair when vehicle is already at full armor
      (setf (mud.world:vehicle-armor vehicle-template) 10)
      (multiple-value-bind (success message death-occurred)
          (mud.combat:cast-spell player player "repair")
        (if success
            (format t "✓ Repair spell handles already-repaired vehicle: ~a~%" message)
            (format t "✗ Repair spell failed on full vehicle: ~a~%" message))))
    
    ;; Test repair without vehicle
    (setf (mud.player:player-vehicle player) nil)
    (multiple-value-bind (success message death-occurred)
        (mud.combat:cast-spell player player "repair")
      (if (not success)
          (format t "✓ Repair spell correctly fails without vehicle: ~a~%" message)
          (format t "✗ Repair spell should fail without vehicle!~%")))
    
    (format t "~%")))

(defun test-repair-spell-mana-cost ()
  "Test that repair spell costs mana"
  (format t "Testing repair spell mana cost...~%")
  
  (let* ((player (mud.player:make-player :name "TestPlayer"))
         (vehicle-item (mud.inventory:make-item :name "test-car" :type :vehicle)))
    
    ;; Set low mana for testing
    (setf (mud.player:player-mana player) 5)
    (setf (mud.player:player-vehicle player) vehicle-item)
    (mud.world:define-vehicle "test-car" :ground "A test car" :armor 5)
    
    (format t "  Player mana before: ~d~%" (mud.player:player-mana player))
    
    (multiple-value-bind (success message death-occurred)
        (mud.combat:cast-spell player player "repair")
      (if (not success)
          (format t "✓ Repair spell correctly fails with insufficient mana: ~a~%" message)
          (format t "✗ Repair spell should fail with insufficient mana!~%")))
    
    (format t "~%")))

(defun run-repair-spell-tests ()
  "Run all repair spell tests"
  (format t "=== Repair Spell Tests ===~%~%")
  
  (test-repair-spell-basic)
  (test-repair-spell-logic)
  (test-repair-spell-mana-cost)
  
  (format t "=== Tests Complete ===~%"))

;; Run the tests
(run-repair-spell-tests)
