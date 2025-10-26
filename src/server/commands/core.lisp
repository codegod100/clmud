(in-package :mud.server)

(defun parse-command (line)
  (let* ((trimmed (string-trim *whitespace-chars* line)))
    (if (zerop (length trimmed))
        (values nil "")
        (let ((split (position-if #'whitespace-char-p trimmed)))
          (if split
              (values (string-downcase (subseq trimmed 0 split))
                      (string-left-trim *whitespace-chars*
                                        (subseq trimmed split)))
              (values (string-downcase trimmed) ""))))))

(defun normalize-direction (dir-string)
  "Convert direction string to keyword, handling single-letter aliases"
  (let ((dir (string-downcase dir-string)))
    (cond ((string= dir "n") :north) ((string= dir "s") :south)
          ((string= dir "e") :east) ((string= dir "w") :west)
          ((string= dir "u") :up) ((string= dir "d") :down)
          ((string= dir "ne") :northeast) ((string= dir "nw") :northwest)
          ((string= dir "se") :southeast) ((string= dir "sw") :southwest)
          (t (intern (string-upcase dir) :keyword)))))

(defparameter *command-dispatch* (make-hash-table :test #'equal))

(defun register-command-handler (names handler)
  (dolist (name names)
    (setf (gethash (string-downcase name) *command-dispatch*) handler)))

(defmacro define-command ((names function-name) (player rest) &body body)
  `(progn
     (defun ,function-name (,player ,rest)
       ,@body)
     (register-command-handler ',names #',function-name)))

(defun register-direction-command (name)
  (let ((direction (normalize-direction name)))
    (register-command-handler (list name)
      (lambda (player rest)
        (declare (ignore rest))
        (move-player player direction)))))

(defun handle-command (player line)
  (multiple-value-bind (verb rest)
      (parse-command line)
    (cond
     ((null verb) (send-room-overview player))
     (t
      (let ((handler (gethash verb *command-dispatch*)))
        (if handler
            (funcall handler player rest)
            (write-crlf (player-stream player)
             (wrap "Unknown command." :bright-red))))))))
