(defpackage :mud.ansi
  (:use :cl)
  (:export :wrap :code :strip :gradient))

(defpackage :mud.world
  (:use :cl)
  (:shadow :room)
  (:export :initialize-world :find-room :starting-room :neighbor :room-id :room-name :room-description :room-exits
           :add-item-to-room :remove-item-from-room :find-item-in-room :list-room-items :room-items :generate-map))

(defpackage :mud.player
  (:use :cl)
  (:export :make-player :player-name :player-room :set-player-room :player-stream :player-socket
           :player-health :player-max-health :player-mana :player-max-mana :player-level
           :set-player-health :set-player-mana :modify-health :modify-mana
           :player-alive-p :player-inventory))

(defpackage :mud.inventory
  (:use :cl)
  (:export :make-item :item-name :item-type :item-effect :item-value :item-description
           :*item-templates* :find-item-template :create-item :use-item
           :add-to-inventory :remove-from-inventory :find-in-inventory
           :list-inventory :drop-item :grab-item :item-type))

(defpackage :mud.combat
  (:use :cl)
  (:export :*spells* :find-spell :cast-spell :spell-name :spell-cost :spell-damage :spell-description
           :get-player-stats :respawn-player :loot-corpse :handle-player-death))

(defpackage :mud.server
  (:use :cl :sb-bsd-sockets :sb-thread)
  (:import-from :mud.ansi :wrap :code :strip :gradient)
  (:import-from :mud.world :initialize-world :find-room :starting-room :neighbor :room-id :room-name :room-description :room-exits
                           :add-item-to-room :remove-item-from-room :find-item-in-room :list-room-items :generate-map)
  (:import-from :mud.player :make-player :player-name :player-room :set-player-room :player-stream :player-socket
                             :player-health :player-max-health :player-mana :player-max-mana :player-level
                             :modify-health :modify-mana :player-alive-p :player-inventory)
  (:import-from :mud.combat :find-spell :cast-spell :spell-name :spell-cost :spell-damage :spell-description
                            :get-player-stats :respawn-player :*spells* :loot-corpse :handle-player-death)
  (:import-from :mud.inventory :create-item :use-item :add-to-inventory :remove-from-inventory
                               :find-in-inventory :list-inventory :item-name :drop-item :grab-item :item-type)
  (:export :start :stop :await))
