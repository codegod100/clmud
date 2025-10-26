(in-package :mud.server)

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
       (wrap "Buy what? Usage: buy <item> [from <merchant>]" :bright-red))
      (return-from command-buy nil))
    (let* ((lower (string-downcase input))
           (from-pos (search " from " lower))
           (item-name (string-trim '(#\Space #\Tab)
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
        (multiple-value-bind (success message item-id)
            (merchant-buy-item merchant player item-name)
          (write-crlf stream (wrap message (if success :bright-green :bright-red)))
          (unless success (return-from command-buy nil))
          (write-crlf stream
           (wrap (format nil "Gold remaining: ~d" (player-gold player))
                 :bright-cyan))
          (when item-id
            (announce-to-room player
             (format nil "~a buys ~a from ~a."
                     (wrap (player-name player) :bright-yellow)
                     (wrap item-id :bright-green)
                     (wrap (merchant-name merchant) :bright-blue))
             :include-self nil))))))
  )

(define-command (("sell") command-sell) (player rest)
  (let* ((stream (player-stream player))
         (input (string-trim '(#\Space #\Tab) rest)))
    (when (zerop (length input))
      (write-crlf stream
       (wrap "Sell what? Usage: sell <item> [to <merchant>]" :bright-red))
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
            :include-self nil))))))
