(load "src/packages.lisp")
(load "src/final-map.lisp")
(load "src/world.lisp")

(format t "=== Simple Map Test ===~%~%")

;; Test the map function directly
(let ((map-output (mud.world:generate-artistic-map 'village-square)))
  (format t "Map output: ~a~%" map-output)
  (format t "Map output length: ~a~%" (length (or map-output ""))))

(format t "~%Test completed!~%")