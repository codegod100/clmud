(in-package :mud.server)

;; Load all command modules
(load (merge-pathnames "core.lisp" *load-truename*))
(load (merge-pathnames "movement.lisp" *load-truename*))
(load (merge-pathnames "combat.lisp" *load-truename*))
(load (merge-pathnames "inventory.lisp" *load-truename*))
(load (merge-pathnames "trade.lisp" *load-truename*))
(load (merge-pathnames "vehicle.lisp" *load-truename*))
(load (merge-pathnames "player.lisp" *load-truename*))
