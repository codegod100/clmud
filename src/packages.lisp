(defpackage :mud.ansi
  (:use :cl)
  (:export :wrap :code :strip :gradient))

(defpackage :mud.world
  (:use :cl)
  (:export :initialize-world :find-room :starting-room :neighbor :room-id :room-name :room-description :room-exits))

(defpackage :mud.player
  (:use :cl)
  (:export :make-player :player-name :player-room :set-player-room :player-stream :player-socket))

(defpackage :mud.server
  (:use :cl :sb-bsd-sockets :sb-thread)
  (:import-from :mud.ansi :wrap :code :strip :gradient)
  (:import-from :mud.world :initialize-world :find-room :starting-room :neighbor :room-id :room-name :room-description :room-exits)
  (:import-from :mud.player :make-player :player-name :player-room :set-player-room :player-stream :player-socket)
  (:export :start :stop :await))
