(require :sb-bsd-sockets)

(defpackage :mud.constants
  (:use :cl)
  (:export :*tick-interval*))

(defpackage :mud.events
  (:use :cl)
  (:export :*tick-handlers*
           :register-tick-handler
           :process-tick-events))

(in-package :mud.constants)

;; Global tick interval for the MUD system (in seconds)
(defparameter *tick-interval* 5
  "The base tick interval for the MUD system in seconds. This controls:
   - Mob movement frequency
   - Combat round frequency  
   - Autofight timing
   - Other periodic game events")

(in-package :mud.events)

;; Global tick event handlers
(defparameter *tick-handlers* nil
  "List of functions to call on each tick")

(defun register-tick-handler (handler)
  "Register a function to be called on each tick"
  (push handler *tick-handlers*))

(defun process-tick-events ()
  "Process all registered tick event handlers"
  (dolist (handler *tick-handlers*)
    (handler-case
        (funcall handler)
      (error (e)
        (format t "Error in tick handler: ~a~%" e)))))

(defpackage :mud.ansi
  (:use :cl)
  (:export :wrap :code :strip :gradient))

(defpackage :mud.world
  (:use :cl)
  (:shadow :room)
  (:export :initialize-world
           :find-room
           :find-room-by-name
           :starting-room
           :neighbor
           :room-id
           :room-name
           :room-description
           :room-exits
           :add-item-to-room
           :remove-item-from-room
           :find-item-in-room
           :find-facet-in-room
           :list-room-items
           :room-items
           :direction-delta
           :explore-map
           :generate-map
           :generate-artistic-map
           :define-vehicle
           :find-vehicle
           :vehicle-armor
           :vehicle-damage
           :vehicle-speed
           :vehicle-name
           :vehicle-type
           :vehicle-description
           :vehicle-max-armor
           :get-world-time
           :set-world-time
           :advance-world-time
           :get-time-of-day
           :format-world-time
           :is-daytime-p
           :is-nighttime-p
           :get-time-based-description
           :add-time-description
           :define-room-with-time
           :get-time-based-name
           :add-time-name
           :get-global-tick
           :advance-global-tick
           :get-tick-rate
           :set-tick-rate))


(defpackage :mud.player
  (:use :cl)
  (:export :make-player
           :get-or-create-player
           :player-name
           :player-room
           :set-player-room
           :player-stream
           :player-socket
           :player-health
           :player-max-health
           :set-player-health
           :player-mana
           :player-max-mana
           :set-player-mana
           :player-level
           :player-xp
           :player-inventory
           :player-equipped-weapon
           :player-equipped-armor
           :player-vehicle
           :player-quest-state
           :player-gold
           :set-player-gold
           :player-p
           :player-alive-p
           :get-player-damage
           :get-player-armor
           :get-vehicle-armor
           :damage-vehicle
           :vehicle-broken-p
           :attempt-auto-repair
           :modify-health
           :modify-mana
           :modify-gold
           :award-xp
           :xp-for-level
           :xp-to-next-level
           :equip-item
           :unequip-item
           :collect-player-snapshots
           :restore-player-snapshots
           :detach-player
           :show-simple-status
           :%serialize-player
           :get-faction-standing
           :set-faction-standing
           :modify-faction-standing
           :get-faction-reputation
           :list-faction-standings))

(defpackage :mud.merchant
  (:use :cl)
  (:export :initialize-merchants
           :get-merchants-in-room
           :find-merchant
           :find-merchant-in-room-by-name
           :merchant-buy-item
           :merchant-sell-item
           :merchant-name
           :merchant-description
           :merchant-greeting
           :merchant-room-id
           :merchant-stock-summary))

(defpackage :mud.inventory
  (:use :cl)
  (:export :make-item
           :duplicate-item
           :item-name
           :item-type
           :item-effect
           :item-value
           :item-description
           :item-vehicle-type
           :item-portable
           :item-damage
           :item-armor
           :item-slot
           :find-item-template
           :create-item
           :add-to-inventory
           :remove-from-inventory
           :find-in-inventory
           :find-equipped-item
           :list-inventory
           :use-item
           :drop-item
           :grab-item
           :quest-item-p))

(defpackage :mud.mob
  (:use :cl)
  (:import-from :mud.constants :*tick-interval*)
  (:export :define-mob-template
           :find-mob-template
           :spawn-mob
           :get-mobs-in-room
           :find-mob-in-room
           :remove-mob-from-room
           :mob-alive-p
           :damage-mob
           :get-mob-loot
           :initialize-mobs
           :mob-name
           :mob-description
           :mob-health
           :mob-max-health
           :mob-damage
           :mob-armor
           :mob-xp-reward
           :mob-current-room
           :mob-last-move-time
           :mob-move-interval
           :process-all-mob-movements
           :move-mob-to-room
           :mob-aggressive-p
           :get-aggressive-mobs-in-room
           :should-mob-attack-player
           :start-combat
           :end-combat
           :mob-in-combat-p
           :process-all-mob-combat))

(defpackage :mud.combat
  (:use :cl)
  (:export :cast-spell
           :find-spell
           :create-corpse
           :loot-corpse
           :handle-player-death
           :get-player-stats
           :respawn-player
            :*corpse-data*
            :*spells*
            :spell-name
            :spell-cost
            :spell-damage
            :spell-description))

(defpackage :mud.quest
  (:use :cl)
  (:export :define-quest
           :find-quest
           :start-quest
           :check-quest-completion
           :get-active-quests
           :get-completed-quests
           :has-item-in-inventory-p
           :initialize-quests
           :quest-name
           :quest-description
           :quest-reward-xp
           :quest-reward-items
           :quest-reward-text
           :maybe-announce-quest-rewards))

(defpackage :mud.game-state
  (:use :cl :mud.player :mud.world)
  (:export :game-state :make-game-state :game-state-timestamp :game-state-global-tick :game-state-players
           :save-game-state :load-game-state))

(defpackage :mud.server
  (:use :cl
        :sb-bsd-sockets
        :sb-thread
        :mud.ansi
        :mud.world
        :mud.player
        :mud.inventory
        :mud.mob
        :mud.combat
        :mud.quest
        :mud.merchant
        :mud.game-state)
  (:shadow :log :save-game-state)
  (:export :start :stop :await
           :handle-aggressive-mob-attack
           :handle-mob-attack-player
           :announce-to-room
           :send-room-overview
           :move-player
           :current-room
           :write-crlf
           :get-vehicle-type-description
           :get-required-vehicle-type
           :get-vehicles-with-players
           :save-game-state))
