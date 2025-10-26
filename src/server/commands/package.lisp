(in-package :mud.server)

;; Load all command modules
(load "src/server/commands/core.lisp")
(load "src/server/commands/movement.lisp")
(load "src/server/commands/combat.lisp")
(load "src/server/commands/inventory.lisp")
(load "src/server/commands/trade.lisp")
(load "src/server/commands/vehicle.lisp")
(load "src/server/commands/player.lisp")
