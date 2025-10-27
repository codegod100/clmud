(in-package :mud.server)

(defun item-slot-score (item slot)
  (cond
    ((null item) 0)
    ((eq slot :weapon) (item-damage item))
    ((eq slot :armor) (item-armor item))
    (t 0)))

(defun best-inventory-item-for-slot (player slot)
  (let* ((inventory (player-inventory player))
         (candidates (remove-if-not (lambda (item)
                                      (eq (item-slot item) slot))
                                    inventory)))
    (when candidates
      (reduce (lambda (best candidate)
                (if (> (item-slot-score candidate slot)
                       (item-slot-score best slot))
                    candidate
                    best))
              (rest candidates)
              :initial-value (first candidates)))))

(defun handle-equip-all (player)
  (let ((equipped 0)
        (stream (player-stream player)))
    (labels ((equip-slot (slot accessor)
               (let* ((candidate (best-inventory-item-for-slot player slot))
                      (current (funcall accessor player))
                      (candidate-score (item-slot-score candidate slot))
                      (current-score (item-slot-score current slot)))
                 (when (and candidate
                            (or (null current)
                                (> candidate-score current-score)))
                   (remove-from-inventory player candidate)
                   (multiple-value-bind (success message)
                       (equip-item player candidate)
                     (if success
                         (progn
                           (incf equipped)
                           (write-crlf stream (wrap message :bright-green))
                           (mud.server::announce-to-room player
                            (format nil "~a equips ~a."
                                    (wrap (player-name player)
                                     :bright-yellow)
                                    (item-name candidate))
                            :include-self nil))
                         (progn
                           (add-to-inventory player candidate)
                           (write-crlf stream (wrap message :bright-red)))))))))
      (equip-slot :weapon #'player-equipped-weapon)
      (equip-slot :armor #'player-equipped-armor)
      (when (zerop equipped)
        (write-crlf stream
         (wrap "You have nothing better to equip." :bright-red))))))

(defun handle-get-all (player)
  (let* ((room-id (player-room player))
         (room (find-room room-id))
         (stream (player-stream player))
         (items (when room (copy-list (room-items room))))
         (collected 0))
    (cond
      ((null room)
       (write-crlf stream
        (wrap "You are nowhere. There's nothing to take." :bright-red)))
      ((null items)
       (write-crlf stream (wrap "There is nothing here to take." :bright-red)))
      (t
       (dolist (item items)
         (let ((target-name (mud.inventory::item-name item)))
           (if (eq (mud.inventory::item-type item) :corpse)
               (let ((corpse-items (mud.combat::loot-corpse item)))
                 (if corpse-items
                     (progn
                       (dolist (corpse-item corpse-items)
                         (mud.inventory::add-to-inventory player corpse-item)
                         (incf collected)
                         (mud.server::maybe-announce-quest-rewards player))
                       (mud.world::remove-item-from-room room-id item)
                       (write-crlf stream
                        (wrap
                         (format nil
                                 "You loot the corpse and take ~d item~:p."
                                 (length corpse-items))
                         :bright-green))
                       (mud.server::announce-to-room player
                        (format nil "~a loots ~a."
                                (wrap (player-name player)
                                 :bright-yellow)
                                target-name)
                        :include-self nil))
                     (write-crlf stream
                      (wrap "The corpse is empty." :bright-red))))
               ;; Skip vehicles silently - only try to grab portable items
               (if (mud.inventory::item-portable item)
                   (multiple-value-bind (success message)
                       (mud.inventory::grab-item player target-name)
                     (if success
                         (progn
                           (incf collected)
                           (write-crlf stream (wrap message :bright-green))
                           (mud.server::announce-to-room player
                            (format nil "~a gets ~a."
                                    (wrap (player-name player)
                                     :bright-yellow)
                                    target-name)
                            :include-self nil)
                           (mud.server::maybe-announce-quest-rewards player))
                         (write-crlf stream (wrap message :bright-red))))
                   ;; Silently skip non-portable items (like vehicles)
                   nil))))
       (when (zerop collected)
         (write-crlf stream
          (wrap "You fail to pick up anything." :bright-red)))))))

(define-command (("inventory" "inv" "i") command-inventory) (player rest)
  (declare (ignore rest))
  (write-crlf (player-stream player) (list-inventory player)))

(define-command (("equip") command-equip) (player rest)
  (let ((item-name (string-trim '(#\  #\Tab) rest)))
    (cond
      ((zerop (length item-name))
       (handle-equip-all player))
      ((string-equal item-name "all")
       (handle-equip-all player))
      (t
       (let ((item (find-in-inventory player item-name)))
         (if item
             (progn
               (remove-from-inventory player item)
               (multiple-value-bind (success message)
                   (equip-item player item)
                 (write-crlf (player-stream player)
                  (wrap message (if success :bright-green :bright-red)))
                 (if success
                     (mud.server::announce-to-room player
                      (format nil "~a equips ~a."
                              (wrap (player-name player) :bright-yellow)
                              item-name)
                      :include-self nil)
                     (add-to-inventory player item))))
             (write-crlf (player-stream player)
              (wrap (format nil "You don't have any ~a." item-name)
               :bright-red))))))))

(define-command (("unequip") command-unequip) (player rest)
  (if (zerop (length rest))
      (write-crlf (player-stream player)
       (wrap "Unequip what? Usage: unequip weapon/armor" :bright-red))
      (let* ((slot-name (string-trim '(#\  #\Tab) rest))
             (slot (cond ((or (string-equal slot-name "weapon")
                              (string-equal slot-name "w"))
                          :weapon)
                         ((or (string-equal slot-name "armor")
                              (string-equal slot-name "a"))
                          :armor)
                         (t nil))))
        (if slot
            (multiple-value-bind (success message)
                (unequip-item player slot)
              (write-crlf (player-stream player)
               (wrap message (if success :bright-green :bright-red)))
              (when success
                (announce-to-room player
                 (format nil "~a unequips ~a."
                         (wrap (player-name player) :bright-yellow)
                         slot-name)
                 :include-self nil)))
            (write-crlf (player-stream player)
             (wrap
              "Unequip weapon or armor? Usage: unequip weapon/armor"
              :bright-red))))))

(define-command (("use") command-use) (player rest)
  (if (zerop (length rest))
      (write-crlf (player-stream player)
       (wrap "Use what? Usage: use <item>" :bright-red))
      (let ((item-name (string-trim '(#\  #\Tab) rest)))
        (multiple-value-bind (success message action-type old-vehicle new-vehicle)
            (use-item player item-name)
          (if success
              (progn
                (write-crlf (player-stream player)
                 (wrap message :bright-green))
                ;; Handle vehicle announcements
                (let ((item (or (mud.inventory::find-in-inventory player item-name)
                                (mud.world:find-item-in-room (mud.player:player-room player) item-name))))
                  (when (and item (eq (mud.inventory::item-type item) :vehicle))
                    (case action-type
                      (:enter
                       ;; Player entered a vehicle
                       (announce-to-room player
                        (format nil "~a enters ~a."
                                (wrap (mud.player:player-name player) :bright-blue)
                                (mud.inventory::item-name new-vehicle))
                        :include-self nil))
                      (:exit
                       ;; Player exited a vehicle
                       (announce-to-room player
                        (format nil "~a exits ~a."
                                (wrap (mud.player:player-name player) :bright-blue)
                                (mud.inventory::item-name old-vehicle))
                        :include-self nil))
                      (:transfer
                       ;; Player transferred vehicles - announce both exit and entry
                       (announce-to-room player
                        (format nil "~a exits ~a."
                                (wrap (mud.player:player-name player) :bright-blue)
                                (mud.inventory::item-name old-vehicle))
                        :include-self nil)
                       (announce-to-room player
                        (format nil "~a enters ~a."
                                (wrap (mud.player:player-name player) :bright-blue)
                                (mud.inventory::item-name new-vehicle))
                        :include-self nil)))
                    (send-room-overview player))))
              (write-crlf (player-stream player)
               (wrap message :bright-red)))))))

(define-command (("drop") command-drop) (player rest)
  (if (zerop (length rest))
      (write-crlf (player-stream player)
       (wrap "Drop what? Usage: drop <item>" :bright-red))
      (let ((item-name (string-trim '(#\  #\Tab) rest)))
        (multiple-value-bind (success message)
            (drop-item player item-name)
          (if success
              (progn
                (write-crlf (player-stream player)
                 (wrap message :bright-green))
                (announce-to-room player
                 (format nil "~a drops ~a."
                         (wrap (player-name player) :bright-yellow)
                         item-name)
                 :include-self nil))
              (write-crlf (player-stream player)
               (wrap message :bright-red)))))))

(define-command (("get" "grab") command-get) (player rest)
  (let ((item-name (string-trim '(#\  #\Tab) rest)))
    (cond
      ((zerop (length item-name))
       (write-crlf (player-stream player)
        (wrap "Get what? Usage: get <item>" :bright-red)))
      ((string-equal item-name "all")
       (handle-get-all player))
      (t
       (let ((item (mud.world::find-item-in-room (player-room player) item-name)))
         (if (and item (eq (mud.inventory::item-type item) :corpse))
             (let ((corpse-items (mud.combat::loot-corpse item)))
               (if corpse-items
                   (progn
                     (dolist (corpse-item corpse-items)
                       (mud.inventory::add-to-inventory player corpse-item)
                       (mud.server::maybe-announce-quest-rewards player))
                     (mud.world::remove-item-from-room (player-room player) item)
                     (write-crlf (player-stream player)
                      (wrap
                       (format nil
                               "You loot the corpse and take ~d item~:p."
                               (length corpse-items))
                       :bright-green))
                     (mud.server::announce-to-room player
                      (format nil "~a loots ~a."
                              (wrap (player-name player) :bright-yellow)
                              item-name)
                      :include-self nil))
                   (write-crlf (player-stream player)
                    (wrap "The corpse is empty." :bright-red))))
             (multiple-value-bind (success message)
                 (mud.inventory::grab-item player item-name)
               (if success
                   (progn
                     (write-crlf (player-stream player)
                      (wrap message :bright-green))
                     (mud.server::announce-to-room player
                      (format nil "~a gets ~a."
                              (wrap (player-name player) :bright-yellow)
                              item-name)
                      :include-self nil)
                     (mud.server::maybe-announce-quest-rewards player))
                   (write-crlf (player-stream player)
                    (wrap message :bright-red))))))))))
