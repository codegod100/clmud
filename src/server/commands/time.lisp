(in-package :mud.server)

(define-command (("time") command-time) (player rest)
  (let ((stream (player-stream player)))
    (if (zerop (length (string-trim *whitespace-chars* rest)))
        ;; No argument - show current time
        (progn
          (write-crlf stream
           (wrap (format nil "Current time: ~a" (mud.world:format-world-time)) :bright-yellow))
          (write-crlf stream
           (wrap (format nil "Tick rate: ~d ticks/hour (~d seconds/tick)" 
                         (mud.world:get-tick-rate)
                         mud.constants::*tick-interval*) :bright-cyan)))
        ;; Argument provided - set time
        (let* ((time-str (string-trim *whitespace-chars* rest))
               (colon-pos (position #\: time-str)))
          (if colon-pos
              ;; Format: HH:MM
              (let* ((hours-str (subseq time-str 0 colon-pos))
                     (minutes-str (subseq time-str (1+ colon-pos)))
                     (hours (ignore-errors (parse-integer hours-str)))
                     (minutes (ignore-errors (parse-integer minutes-str))))
                (if (and hours minutes (>= hours 0) (< hours 24) (>= minutes 0) (< minutes 60))
                    (let ((new-time (+ hours (/ minutes 60.0))))
                      (mud.world:set-world-time new-time)
                      (write-crlf stream
                       (wrap (format nil "Time set to: ~a" (mud.world:format-world-time)) :bright-green)))
                    (write-crlf stream
                     (wrap "Invalid time format. Use HH:MM (e.g., 14:30)" :bright-red))))
              ;; Format: just hours
              (let ((hours (ignore-errors (parse-integer time-str))))
                (if (and hours (>= hours 0) (< hours 24))
                    (progn
                      (mud.world:set-world-time (float hours))
                      (write-crlf stream
                       (wrap (format nil "Time set to: ~a" (mud.world:format-world-time)) :bright-green)))
                    (write-crlf stream
                     (wrap "Invalid time format. Use HH:MM or just HH (e.g., 14:30 or 14)" :bright-red)))))))))