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
           :vehicle-max-armor))

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
           :%serialize-player))

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
           :grab-item))

(defpackage :mud.mob
  (:use :cl)
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
           :has-item-in-inventory-p
           :initialize-quests
           :quest-name
           :quest-description
           :quest-reward-xp
           :quest-reward-text
           :maybe-announce-quest-rewards))

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
    :mud.merchant)
  (:shadow :log)
  (:export :start :stop :await
           :get-vehicles-with-players))
