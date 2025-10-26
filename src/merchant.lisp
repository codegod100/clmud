
(in-package :mud.merchant)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (declaim (ftype (function (t) integer) mud.player:player-gold)
           (ftype (function (t integer) *) mud.player:modify-gold)
           (ftype (function (t) t) mud.player:player-equipped-weapon)
           (ftype (function (t) t) mud.player:player-equipped-armor)
           (ftype (function (t t) t) mud.inventory:find-in-inventory)
           (ftype (function (t t) *) mud.inventory:remove-from-inventory)
           (ftype (function (t t) *) mud.inventory:add-to-inventory)
           (ftype (function (t) string) mud.inventory:item-name)
           (ftype (function (t) t) mud.inventory:create-item)
           (ftype (function (t) t) mud.inventory:find-item-template)
           (ftype (function (t) string) mud.inventory:item-description)))

(defparameter *merchants* (make-hash-table :test 'eq)
  "Maps merchant identifiers to merchant structs.")

(defstruct (merchant-stock
            (:constructor make-merchant-stock (&key template price (quantity :infinite)
                                                    buy-value description)))
  template
  price
  quantity
  buy-value
  description)

(defstruct (merchant
            (:constructor %make-merchant (&key id name room-id description greeting
                                               (stock nil) (gold :infinite))))
  id
  name
  room-id
  description
  greeting
  stock
  gold)

(defun clear-merchants ()
  (clrhash *merchants*))

(defun %coerce-stock (stock)
  (when stock (mapcar #'copy-structure stock)))

(defmacro define-merchant (id &key name room-id description greeting stock gold)
  `(setf (gethash ,id *merchants*)
         (%make-merchant :id ,id
                         :name ,name
                         :room-id ,room-id
                         :description ,description
                         :greeting ,greeting
                         :stock (%coerce-stock ,stock)
                         :gold (or ,gold :infinite))))

(defun all-merchants ()
  (loop for merchant being the hash-values of *merchants*
        collect merchant))

(defun get-merchants-in-room (room-id)
  (remove-if-not (lambda (merchant)
                   (eq (merchant-room-id merchant) room-id))
                 (all-merchants)))

(defun find-merchant (id)
  (gethash id *merchants*))

(defun %normalize-item-name (name)
  (let* ((lower (string-downcase name)))
    (remove-if (lambda (char)
                 (or (char= char #\space)
                     (char= char #\-)
                     (char= char #\')))
               lower)))

(defun find-merchant-in-room-by-name (room-id name)
  (let ((target (string-downcase (string-trim '(#\Space #\Tab) name))))
    (find-if (lambda (merchant)
               (let ((norm-merchant (%normalize-item-name (merchant-name merchant)))
                     (target-norm (%normalize-item-name target)))
                 (or (string= norm-merchant target-norm)
                     (search target-norm norm-merchant))))
             (get-merchants-in-room room-id))))

(defun %stock-entry-matches-p (entry item-name)
  (let ((template (merchant-stock-template entry)))
    (when template
      (let ((template-norm (%normalize-item-name template))
            (query-norm (%normalize-item-name item-name)))
        (or (string= template-norm query-norm)
            (search query-norm template-norm))))))

(defun %find-stock-entry (merchant item-name)
  (find-if (lambda (entry)
             (%stock-entry-matches-p entry item-name))
           (merchant-stock merchant)))

(defun %stock-entry-available-p (entry)
  (let ((quantity (merchant-stock-quantity entry)))
    (or (eq quantity :infinite)
        (> quantity 0))))

(defun %decrement-stock (entry)
  (let ((quantity (merchant-stock-quantity entry)))
    (when (and quantity (not (eq quantity :infinite)))
      (setf (merchant-stock-quantity entry) (max 0 (1- quantity))))))

(defun %increment-stock (entry)
  (let ((quantity (merchant-stock-quantity entry)))
    (if (eq quantity :infinite)
        entry
        (setf (merchant-stock-quantity entry) (1+ (or quantity 0))))))

(defun %merchant-gold-sufficient-p (merchant amount)
  (let ((gold (merchant-gold merchant)))
    (or (eq gold :infinite)
        (>= gold amount))))

(defun %credit-merchant (merchant amount)
  (let ((gold (merchant-gold merchant)))
    (unless (eq gold :infinite)
      (setf (merchant-gold merchant) (+ gold amount)))))

(defun %debit-merchant (merchant amount)
  (let ((gold (merchant-gold merchant)))
    (unless (eq gold :infinite)
      (setf (merchant-gold merchant) (max 0 (- gold amount))))))

(defun merchant-stock-summary (merchant)
  (let ((sellable (remove-if-not #'merchant-stock-price (merchant-stock merchant))))
    (if sellable
        (with-output-to-string (out)
          (dolist (entry sellable)
            (let* ((name (merchant-stock-template entry))
                   (price (merchant-stock-price entry))
                   (quantity (merchant-stock-quantity entry))
                   (template (and name (mud.inventory:find-item-template name)))
                   (detail (or (merchant-stock-description entry)
                               (and template (mud.inventory:item-description template)))))
              (format out "  ~a - ~d gold" name price)
              (when (and quantity (not (eq quantity :infinite)))
                (format out " (x~d left)" quantity))
              (when detail
                (format out "~%     ~a" detail))
              (format out "~%"))))
        "  Nothing for sale right now.")))

(defun %determine-buy-value (entry)
  (or (merchant-stock-buy-value entry)
      (let ((price (merchant-stock-price entry)))
        (when price (max 1 (floor (* price 0.5)))))))

(defun merchant-buy-item (merchant player item-name)
  (let* ((entry (%find-stock-entry merchant item-name))
         (price (and entry (merchant-stock-price entry))))
    (cond
      ((null entry)
       (values nil "They don't stock that item." nil))
      ((null price)
       (values nil "That item isn't for sale." nil))
      ((not (%stock-entry-available-p entry))
       (values nil "That item is currently sold out." nil))
      ((< (mud.player:player-gold player) price)
       (values nil
               (format nil "You need ~d more gold." (- price (mud.player:player-gold player)))
               nil))
      (t
       (let* ((template (merchant-stock-template entry))
              (item (and template (mud.inventory:create-item template))))
         (cond
           ((null template)
            (values nil "The merchant fumbles with their ledgers and cannot sell that now." nil))
           ((null item)
            (values nil "The merchant can't find that stock today." nil))
           (t
            (mud.player:modify-gold player (- price))
      (mud.inventory:add-to-inventory player item)
      (%decrement-stock entry)
      (%credit-merchant merchant price)
      (values t
        (format nil "You buy ~a for ~d gold." (mud.inventory:item-name item) price)
        (mud.inventory:item-name item)))))))))

(defun merchant-sell-item (merchant player item-name)
  (let ((item (mud.inventory:find-in-inventory player item-name)))
    (cond
      ((null item)
       (values nil (format nil "You don't have any ~a." item-name) nil))
      ((eq item (mud.player:player-equipped-weapon player))
       (values nil "You'll need to unequip that weapon first." nil))
      ((eq item (mud.player:player-equipped-armor player))
       (values nil "You'll need to unequip that armor first." nil))
      (t
       (let* ((entry (%find-stock-entry merchant (mud.inventory:item-name item)))
              (value (and entry (%determine-buy-value entry))))
         (cond
           ((null entry)
            (values nil "They are not interested in that item." nil))
           ((null value)
            (values nil "They can't offer you gold for that." nil))
           ((not (%merchant-gold-sufficient-p merchant value))
            (values nil "They can't afford to buy that right now." nil))
           (t
            (mud.inventory:remove-from-inventory player item)
            (%increment-stock entry)
            (%debit-merchant merchant value)
            (mud.player:modify-gold player value)
            (values t
                    (format nil "You sell ~a for ~d gold." (mud.inventory:item-name item) value)
                    (mud.inventory:item-name item)))))))))

(defun initialize-merchants ()
  (clear-merchants)
  (define-merchant :lena
    :name "Lena the Trader"
    :room-id 'mud.world::market-stalls
    :description "A shrewd merchant with a warm smile and a keen eye for quality goods."
    :greeting "Lena polishes a brass lantern and beckons you over. \"Care to trade?\""
    :gold 500
    :stock (list (make-merchant-stock :template "mana-potion"
                                      :price 15
                                      :quantity :infinite
                                      :buy-value 8
                                      :description "Restores 25 mana.")
                 (make-merchant-stock :template "health-potion"
                                      :price 20
                                      :quantity :infinite
                                      :buy-value 10
                                      :description "Restores 30 health.")
                 (make-merchant-stock :template "steel-sword"
                                      :price 120
                                      :quantity 2
                                      :buy-value 60
                                      :description "A reliable blade for aspiring heroes.")
                 (make-merchant-stock :template "leather-armor"
                                      :price 90
                                      :quantity 2
                                      :buy-value 45
                                      :description "Sturdy protection favored by rangers.")
                 (make-merchant-stock :template "rusty-dagger"
                                      :price 25
                                      :quantity 5
                                      :buy-value 8
                                      :description "A simple dagger fit for close quarters.")
                 (make-merchant-stock :template "wolf-pelt"
                                      :price nil
                                      :quantity :infinite
                                      :buy-value 12
                                      :description "Lena buys these to send to distant tanners.")
                 (make-merchant-stock :template "repair-kit"
                                      :price 50
                                      :quantity :infinite
                                      :buy-value 25
                                      :description "Essential tools for maintaining vehicles. Fully restores vehicle armor."))))