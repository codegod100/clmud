(in-package :mud.server)

;; Register direction commands
(eval-when (:load-toplevel :execute)
  (dolist (alias '("n" "s" "e" "w" "u" "d" "ne" "nw" "se" "sw" "downstream" "upstream"))
    (register-direction-command alias)))

(define-command (("look" "l") command-look) (player rest)
  (if (zerop (length rest))
      (send-room-overview player)
      (handle-look-at player rest)))
