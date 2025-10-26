#!/usr/bin/env sbcl --script

;; Load required packages first
(require :sb-bsd-sockets)
(require :asdf)

;; Load the MUD system
(load "src/packages.lisp")
(load "src/ansi.lisp")
(load "src/player.lisp")
(load "src/inventory.lisp")
(load "src/merchant.lisp")
(load "src/world.lisp")
(load "src/mob.lisp")
(load "src/combat.lisp")
(load "src/quest.lisp")
(load "src/server/core.lisp")

;; Initialize the world
(mud.world:initialize-world)

;; Set up the command system
(defparameter *command-dispatch* (make-hash-table :test #'equal))

(defun register-command-handler (names handler)
  (dolist (name names)
    (setf (gethash (string-downcase name) *command-dispatch*) handler)))

(defmacro define-command ((names function-name) (player rest) &body body)
  `(progn
     (defun ,function-name (,player ,rest)
       ,@body)
     (register-command-handler ',names #',function-name)))

;; Define handle-get-all function first
(defun handle-get-all (player)
  (format t "handle-get-all called~%"))

;; Try to define the get command with announce-to-room call
(format t "Testing get command with announce-to-room call...~%")

(handler-case
    (progn
      (define-command (("get8" "grab8") command-get8) (player rest)
        (let ((item-name (string-trim '(#\  #\Tab) rest)))
          (cond
            ((zerop (length item-name))
             (write-crlf (player-stream player)
              (wrap "Get what? Usage: get <item>" :bright-red)))
            ((string-equal item-name "all")
             (handle-get-all player))
            (t
             (let ((item (find-item-in-room (player-room player) item-name)))
               (if (and item (eq (item-type item) :corpse))
                   (let ((corpse-items (loot-corpse item)))
                     (if corpse-items
                         (progn
                           (dolist (corpse-item corpse-items)
                             (add-to-inventory player corpse-item)
                             (maybe-announce-quest-rewards player))
                           (remove-item-from-room (player-room player) item)
                           (write-crlf (player-stream player)
                            (wrap
                             (format nil
                                     "You loot the corpse and take ~d item~:p."
                                     (length corpse-items))
                             :bright-green))
                           (announce-to-room player
                            (format nil "~a loots ~a."
                                    (wrap (player-name player) :bright-yellow)
                                    item-name)
                            :include-self nil))
                         (write-crlf (player-stream player)
                          (wrap "The corpse is empty." :bright-red))))
                   (format t "Item is not a corpse: ~a~%" item-name)))))))
      (format t "✓ Get command with announce-to-room defined successfully~%"))
    (error (err)
      (format t "✗ Error defining get command with announce-to-room: ~a~%" err)))

;; Check if it was registered
(let ((handler (gethash "get8" *command-dispatch*)))
  (if handler
      (format t "✓ Get8 command is registered: ~a~%" handler)
      (format t "✗ Get8 command is NOT registered~%")))

(format t "~%Test completed.~%")
