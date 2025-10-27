#!/usr/bin/env sbcl --script

;; Simple test that mobs leave corpses with their inventory when killed

(load "~/quicklisp/setup.lisp")
(ql:quickload :fiveam)

;; Load dependencies
(require :sb-bsd-sockets)
(require :asdf)
(load "../src/packages.lisp")
(load "../src/ansi.lisp")
(load "../src/player.lisp")
(load "../src/inventory.lisp")
(load "../src/world.lisp")
(load "../src/mob.lisp")
(load "../src/combat.lisp")
(load "../src/quest.lisp")
(load "../src/server/core.lisp")
(load "../src/server/commands.lisp")
(load "../src/server/runtime.lisp")

(defpackage :mud.tests
  (:use :cl :fiveam))

(in-package :mud.tests)

(defparameter +test-room-id+ 'mud.world::test-arena)

(defun ensure-test-room ()
  "Create (or reset) a dedicated empty room for deterministic tests."
  (mud.world::define-room +test-room-id+
                          "Test Arena"
                          "A blank chamber used for automated validations."
                          '())
  +test-room-id+)

(defun reset-test-state ()
  "Reset global world/mob state so each test starts clean."
  (mud.world:initialize-world)
  (clrhash mud.mob::*mob-templates*)
  (clrhash mud.mob::*room-mobs*)
  (clrhash mud.combat::*corpse-data*)
  (ensure-test-room))

(defun make-test-mob (&key (id :dummy)
                           (name "training dummy")
                           (health 10)
                           (damage 0)
                           (armor 0)
                           (xp 25)
                           (loot '()))
  "Create a mob struct with configurable stats."
  (mud.mob::make-mob :id id
                     :name name
                     :description "A motionless target."
                     :health health
                     :max-health health
                     :damage damage
                     :armor armor
                     :xp-reward xp
                     :loot-table loot
                     :inventory nil
                     :aggressive nil))

(defun place-mob-in-room (mob room-id)
  "Place MOB into ROOM-ID so server helpers can find it."
  (push mob (gethash room-id mud.mob::*room-mobs*))
  mob)

(def-suite :mud.tests)
(in-suite :mud.tests)

(test mob-corpse-contains-inventory
  "Test that mobs leave corpses with their inventory when killed"
  (reset-test-state)
  (let* ((room (ensure-test-room))
         (mob (make-test-mob :id :test-mob
                             :name "test mob"
                             :health 10
                             :damage 5
                             :armor 0
                             :xp 25
                             :loot '()))
         (test-item (mud.inventory::create-item "rusty-dagger"))
         (corpse nil))
    
    ;; Place mob in room
    (place-mob-in-room mob room)
    
    ;; Add item to mob's inventory
    (mud.mob::add-item-to-mob-inventory mob test-item)
    
    ;; Verify mob has the item
    (is (= 1 (length (mud.mob::mob-inventory mob))))
    (is (eq test-item (first (mud.mob::mob-inventory mob))))
    
    ;; Kill the mob
    (mud.mob::damage-mob mob 1000)
    (is (not (mud.mob::mob-alive-p mob)))
    
    ;; Create corpse from dead mob
    (setf corpse (mud.combat::create-mob-corpse mob))
    (is (not (null corpse)) "Should create corpse from dead mob")
    
    ;; Verify corpse contains the mob's inventory
    (let ((corpse-items (mud.combat::loot-corpse corpse)))
      (is (= 1 (length corpse-items)))
      (is (eq test-item (first corpse-items))))))

;; Run the test
(format t "~&Running mob corpse inventory test...~%")
(run! 'mob-corpse-contains-inventory)
(format t "✓ Mob corpse inventory test completed~%")
(sb-ext:exit :code 0)
