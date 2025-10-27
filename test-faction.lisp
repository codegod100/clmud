(load "src/packages.lisp")
(load "src/player.lisp")
(load "src/quest.lisp")

;; Test the faction system
(let ((player (mud.player::make-player :name "test" :room 'village-square)))
  (format t "Testing faction system...~%")
  (format t "Initial standing: ~a~%" (mud.player::get-faction-standing player :royal-guard))
  (mud.player::modify-faction-standing player :royal-guard 25)
  (format t "After +25: ~a~%" (mud.player::get-faction-standing player :royal-guard))
  (format t "Reputation: ~a~%" (mud.player::get-faction-reputation player :royal-guard))
  (format t "Test completed successfully!~%"))
