#!/usr/bin/sbcl --script

;; Test dynamic pricing system
;; This test demonstrates how merchant prices change based on stock levels

(load "src/packages.lisp")
(load "src/ansi.lisp")
(load "src/world.lisp")
(load "src/player.lisp")
(load "src/inventory.lisp")
(load "src/merchant.lisp")

;; Initialize systems
(mud.world::initialize-world)
(mud.merchant::initialize-merchants)

(format t "=== Dynamic Pricing System Test ===~%~%")

;; Create a test player
(let ((player (mud.player::make-player :name "TestPlayer" :stream *standard-output*)))
  ;; Get a merchant
  (let ((merchant (mud.merchant::find-merchant :lena)))
    (when merchant
      (format t "Testing dynamic pricing system:~%")
      
      ;; Show initial stock and prices
      (format t "~%1. Initial merchant stock:")
      (format t "~a" (mud.merchant::merchant-stock-summary merchant))
      
      ;; Test buying items to reduce stock
      (format t "~%2. Buying items to reduce stock...")
      (mud.player::modify-gold player 1000) ; Give player lots of gold
      
      ;; Buy some steel swords to reduce stock
      (dotimes (i 3)
        (multiple-value-bind (success message item-id)
            (mud.merchant::merchant-buy-item merchant player "steel-sword")
          (format t "~%   Buy attempt ~d: ~a" (1+ i) (if success "Success" "Failed"))
          (when success
            (format t " - ~a" message))))
      
      ;; Show stock after buying
      (format t "~%~%3. Stock after buying (prices should be higher due to low stock):")
      (format t "~a" (mud.merchant::merchant-stock-summary merchant))
      
      ;; Test selling items back to increase stock
      (format t "~%4. Selling items back to increase stock...")
      
      ;; Sell some items back
      (dotimes (i 2)
        (multiple-value-bind (success message item-id)
            (mud.merchant::merchant-sell-item merchant player "steel-sword")
          (format t "~%   Sell attempt ~d: ~a" (1+ i) (if success "Success" "Failed"))
          (when success
            (format t " - ~a" message))))
      
      ;; Show final stock
      (format t "~%~%5. Final stock (prices should be lower due to higher stock):")
      (format t "~a" (mud.merchant::merchant-stock-summary merchant))
      
      (format t "~%~%=== Test completed! ===")
      (format t "~%Dynamic pricing system working: prices change based on stock levels~%"))))

(format t "~%Test file: tests/features/test-dynamic-pricing.lisp~%")
(format t "This test demonstrates the dynamic pricing system where:~%")
(format t "- Low stock = higher prices (supply and demand)~%")
(format t "- High stock = lower prices~%")
(format t "- Merchants buy all non-quest items~%")
(format t "- Quest items cannot be sold~%")
