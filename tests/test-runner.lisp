(load "~/quicklisp/setup.lisp")
(ql:quickload :fiveam)

;; Load dependencies in the same order as the main entry point, but without
;; starting the server.
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

(defun make-test-player (&key (name "Caster") (room +test-room-id+) (mana 50))
  "Construct a player with a string stream for test assertions."
  (let ((player (mud.player:make-player :name name
                                        :room room
                                        :stream (make-string-output-stream)
                                        :socket nil)))
    (mud.player:set-player-room player room)
    (mud.player:set-player-mana player mana)
    player))

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
                     :aggressive nil))

(defun place-mob-in-room (mob room-id)
  "Place MOB into ROOM-ID so server helpers can find it."
  (push mob (gethash room-id mud.mob::*room-mobs*))
  mob)

(def-suite :mud.tests)
(in-suite :mud.tests)

(test fireball-kills-mob-and-drops-loot
  (reset-test-state)
  (let* ((room (ensure-test-room))
         (player (make-test-player :room room))
         (spell (mud.combat:find-spell "fireball"))
         (mob (make-test-mob :health 10 :xp 42 :loot '("mana-potion"))))
    (place-mob-in-room mob room)
    (is (mud.server::cast-spell-at-mob player mob spell))
    (is (= 35 (mud.player:player-mana player)))
    (is (= 42 (mud.player:player-xp player)))
    (is (null (mud.mob:get-mobs-in-room room)))
    (is (mud.world:find-item-in-room room "mana-potion"))))

(test heal-spell-cannot-target-mobs
  (reset-test-state)
  (let* ((room (ensure-test-room))
         (player (make-test-player :room room :mana 40))
         (spell (mud.combat:find-spell "heal"))
         (mob (make-test-mob :health 12 :damage 0 :loot '())))
    (place-mob-in-room mob room)
    (is (null (mud.server::cast-spell-at-mob player mob spell)))
    (is (= 40 (mud.player:player-mana player)))
    (is (= 12 (mud.mob:mob-health mob)))))

(test surviving-mobs-counterattack
  (reset-test-state)
  (let* ((room (ensure-test-room))
         (player (make-test-player :room room))
         (mob (make-test-mob :health 20 :damage 12 :armor 1 :xp 10)))
    (place-mob-in-room mob room)
    (is (null (mud.server::resolve-mob-hit player mob 5)))
    (is (= 15 (mud.mob:mob-health mob)))
    (is (= 88 (mud.player:player-health player)))
    (is (not (null (mud.mob:get-mobs-in-room room))))))

(format t "~&Running MUD FiveAM tests...~%")
(let ((results (run! :mud.tests)))
  (unless (results-status results)
    (sb-ext:exit :code 1)))
(sb-ext:exit :code 0)
