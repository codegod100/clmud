(in-package :mud.server)

(defun handle-sell-all (player merchant)
  "Handle selling all un-equipped items to a merchant"
  (let* ((stream (player-stream player))
         (inventory (copy-list (mud.player:player-inventory player)))
         (equipped-weapon (mud.player:player-equipped-weapon player))
         (equipped-armor (mud.player:player-equipped-armor player))
         (un-equipped-items (remove-if (lambda (item)
                                         (or (eq item equipped-weapon)
                                             (eq item equipped-armor)))
                                       inventory))
         (sold-count 0)
         (initial-gold (mud.player:player-gold player))
         (failed-items nil))

    (cond
      ((null un-equipped-items)
       (write-crlf stream
        (wrap "You have no un-equipped items to sell." :bright-yellow)))
      (t
       (write-crlf stream
        (wrap (format nil "Attempting to sell ~d un-equipped item~:p..."
                      (length un-equipped-items)) :bright-cyan))

       (dolist (item un-equipped-items)
         (multiple-value-bind (success message item-id)
             (mud.merchant:merchant-sell-item merchant player (mud.inventory:item-name item))
           (if success
               (progn
                 (incf sold-count)
                 (write-crlf stream (wrap message :bright-green)))
               (progn
                 (push (mud.inventory:item-name item) failed-items)
                 (write-crlf stream (wrap message :bright-red))))))

       ;; Summary
       (let ((total-gold-earned (- (mud.player:player-gold player) initial-gold)))
         (write-crlf stream
          (wrap (format nil "Sold ~d item~:p for ~d gold total." sold-count total-gold-earned) :bright-cyan))

                  (when failed-items
                    (let ((item-counts (make-hash-table :test #'equal)))
                      (dolist (item-name failed-items)
                        (incf (gethash item-name item-counts 0)))
                      (let ((counted-items nil))
                        (maphash (lambda (name count)
                                   (push (if (> count 1)
                                             (format nil "~a x~d" name count)
                                             name)
                                         counted-items))
                                 item-counts)
                        (write-crlf stream
                         (wrap (format nil "Could not sell: ~{~a~^, ~}" (nreverse counted-items)) :bright-yellow)))))

         (write-crlf stream
          (wrap (format nil "Gold now: ~d" (mud.player:player-gold player)) :bright-cyan))

         ;; Announce to room
         (when (> sold-count 0)
           (announce-to-room player
            (format nil "~a sells ~d item~:p to ~a."
                    (wrap (mud.player:player-name player) :bright-yellow)
                    sold-count
                    (wrap (mud.merchant:merchant-name merchant) :bright-blue))
            :include-self nil)))))))

(defun show-merchant-to-player (stream merchant player)
  (write-crlf stream
   (wrap (format nil "~a displays their wares." (merchant-name merchant))
         :bright-yellow))
  (when (merchant-description merchant)
    (write-crlf stream (wrap (merchant-description merchant) :bright-white)))
  (when (merchant-greeting merchant)
    (write-crlf stream (wrap (merchant-greeting merchant) :bright-black)))
  (let ((summary (merchant-stock-summary merchant)))
    (when summary
      (write-crlf stream (wrap summary :bright-cyan))))
  (write-crlf stream
   (wrap (format nil "You have ~d gold coins." (player-gold player))
         :bright-green)))

(define-command (("shop") command-shop) (player rest)
  (let* ((stream (player-stream player))
         (room (player-room player))
         (query (string-trim '(#\Space #\Tab) rest))
         (merchants (get-merchants-in-room room)))
    (cond
      ((null merchants)
       (write-crlf stream (wrap "There are no merchants here." :bright-red)))
      ((zerop (length query))
       (if (= (length merchants) 1)
           (show-merchant-to-player stream (first merchants) player)
           (progn
             (write-crlf stream
              (wrap (format nil "Merchants here: ~{~a~^, ~}"
                            (mapcar #'merchant-name merchants))
                    :bright-yellow))
             (write-crlf stream
              (wrap "Use 'shop <merchant>' to browse a specific trader."
                    :bright-magenta)))))
      (t
       (let ((merchant (find-merchant-in-room-by-name room query)))
         (if merchant
             (show-merchant-to-player stream merchant player)
             (write-crlf stream
              (wrap "You don't see that merchant here." :bright-red))))))))

(define-command (("buy") command-buy) (player rest)
  (let* ((stream (player-stream player))
         (input (string-trim '(#\Space #\Tab) rest)))
    (when (zerop (length input))
      (write-crlf stream
       (wrap "Buy what? Usage: buy <item> [<count>] [from <merchant>]" :bright-red))
      (return-from command-buy nil))
    (let* ((lower (string-downcase input))
           (from-pos (search " from " lower))
           (item-part (string-trim '(#\Space #\Tab)
                                   (if from-pos
                                       (subseq input 0 from-pos)
                                       input)))
           (merchant-query (when from-pos
                              (string-trim '(#\Space #\Tab)
                                           (subseq input (+ from-pos 6)))))
           (merchants (get-merchants-in-room (player-room player))))
      (when (null merchants)
        (write-crlf stream
         (wrap "There are no merchants here to buy from." :bright-red))
        (return-from command-buy nil))
      
      ;; Parse count and item name from item-part
      (let* ((words (loop for start = 0 then (1+ pos)
                          for pos = (position #\Space item-part :start start)
                          collect (subseq item-part start (or pos (length item-part)))
                          while pos))
             (count (if (and words (every #'digit-char-p (first words)))
                        (parse-integer (first words) :junk-allowed t)
                        1))
             (item-name (if (and words (every #'digit-char-p (first words)))
                            (format nil "~{~a~^ ~}" (rest words))
                            item-part)))
        (when (or (null count) (<= count 0))
          (write-crlf stream (wrap "Count must be a positive number." :bright-red))
          (return-from command-buy nil))
        (when (zerop (length item-name))
          (write-crlf stream (wrap "Buy what exactly?" :bright-red))
          (return-from command-buy nil))
      (when (and merchant-query (zerop (length merchant-query)))
        (write-crlf stream (wrap "Specify who to buy from." :bright-red))
        (return-from command-buy nil))
      (let ((merchant (cond
                        (merchant-query
                         (find-merchant-in-room-by-name (player-room player)
                                                         merchant-query))
                        ((= (length merchants) 1)
                         (first merchants))
                        (t nil))))
        (unless merchant
          (if (and (not merchant-query) (> (length merchants) 1))
              (write-crlf stream
               (wrap (format nil
                              "Multiple merchants present: ~{~a~^, ~}. Try 'buy <item> from <merchant>'."
                              (mapcar #'merchant-name merchants))
                     :bright-yellow))
              (write-crlf stream
               (wrap "You don't see that merchant here." :bright-red)))
          (return-from command-buy nil))
        ;; Handle multiple purchases
        (let ((total-cost 0)
              (items-bought 0)
              (failed-at nil))
          (loop for i from 1 to count do
            (multiple-value-bind (success message item-id)
                (merchant-buy-item merchant player item-name)
              (if success
                  (progn
                    (incf items-bought)
                    (when item-id
                      (announce-to-room player
                       (format nil "~a buys ~a from ~a."
                               (wrap (player-name player) :bright-yellow)
                               (wrap item-id :bright-green)
                               (wrap (merchant-name merchant) :bright-blue))
                       :include-self nil)))
                  (progn
                    (setf failed-at i)
                    (write-crlf stream (wrap message :bright-red))
                    (return))))
          
          ;; Show summary
          (cond
            ((= items-bought count)
             (write-crlf stream
              (wrap (format nil "Successfully bought ~d ~a~p." count item-name count)
                    :bright-green)))
            ((> items-bought 0)
             (write-crlf stream
              (wrap (format nil "Bought ~d of ~d ~a~p. Stopped at item ~d." 
                            items-bought count item-name count failed-at)
                    :bright-yellow)))
            (t
             (write-crlf stream
              (wrap (format nil "Could not buy any ~a~p." item-name count)
                    :bright-red))))
          
          (write-crlf stream
           (wrap (format nil "Gold remaining: ~d" (player-gold player))
                 :bright-cyan))))))))

(define-command (("sell") command-sell) (player rest)
  (let* ((stream (player-stream player))
         (input (string-trim '(#\Space #\Tab) rest)))
    (when (zerop (length input))
      (write-crlf stream
       (wrap "Sell what? Usage: sell <item> [to <merchant>] or sell all [to <merchant>]" :bright-red))
      (return-from command-sell nil))
    (let* ((lower (string-downcase input))
           (to-pos (search " to " lower))
           (item-name (string-trim '(#\Space #\Tab)
                                   (if to-pos
                                       (subseq input 0 to-pos)
                                       input)))
           (merchant-query (when to-pos
                              (string-trim '(#\Space #\Tab)
                                           (subseq input (+ to-pos 4)))))
           (merchants (get-merchants-in-room (player-room player))))
      (when (null merchants)
        (write-crlf stream
         (wrap "There are no merchants here to sell to." :bright-red))
        (return-from command-sell nil))
      (when (zerop (length item-name))
        (write-crlf stream (wrap "Sell what exactly?" :bright-red))
        (return-from command-sell nil))
      (when (and merchant-query (zerop (length merchant-query)))
        (write-crlf stream (wrap "Specify who to sell to." :bright-red))
        (return-from command-sell nil))
      (let ((merchant (cond
                        (merchant-query
                         (find-merchant-in-room-by-name (player-room player)
                                                         merchant-query))
                        ((= (length merchants) 1)
                         (first merchants))
                        (t nil))))
        (unless merchant
          (if (and (not merchant-query) (> (length merchants) 1))
              (write-crlf stream
               (wrap (format nil
                              "Multiple merchants present: ~{~a~^, ~}. Try 'sell <item> to <merchant>'."
                              (mapcar #'merchant-name merchants))
                     :bright-yellow))
              (write-crlf stream
               (wrap "You don't see that merchant here." :bright-red)))
          (return-from command-sell nil))
        (cond
          ((string-equal item-name "all")
           (handle-sell-all player merchant))
          (t
           (multiple-value-bind (success message item-id)
               (merchant-sell-item merchant player item-name)
             (write-crlf stream (wrap message (if success :bright-green :bright-red)))
             (unless success (return-from command-sell nil))
             (write-crlf stream
              (wrap (format nil "Gold now: ~d" (player-gold player))
                    :bright-cyan))
             (when item-id
               (announce-to-room player
                (format nil "~a sells ~a to ~a."
                        (wrap (player-name player) :bright-yellow)
                        (wrap item-id :bright-green)
                        (wrap (merchant-name merchant) :bright-blue))
                :include-self nil)))))))))

)