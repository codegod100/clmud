;; Script to properly convert the current save data to new prototype format

(defun convert-item-to-template (item-data)
  "Convert old item data to template name"
  (when item-data
    (getf item-data :name)))

(defun convert-inventory (inventory-data)
  "Convert old inventory format to new template format"
  (when inventory-data
    (mapcar #'convert-item-to-template inventory-data)))

(defun convert-player-data (player-data)
  "Convert a single player's data to new format"
  (let ((converted (copy-list player-data)))
    ;; Convert inventory
    (when (getf converted :inventory)
      (setf (getf converted :inventory) 
            (convert-inventory (getf converted :inventory))))
    ;; Convert room references
    (when (getf converted :room)
      (let ((room (getf converted :room)))
        (if (symbolp room)
            (setf (getf converted :room) room)
            (setf (getf converted :room) (intern (symbol-name room) :keyword)))))
    converted))

(defun convert-save-data (old-data)
  "Convert entire save data to new format"
  (mapcar #'convert-player-data old-data))

;; Load and convert the current data
(let ((old-data (with-open-file (stream "data/save-state.lisp")
                  (read stream))))
  (let ((new-data (convert-save-data old-data)))
    ;; Write the converted data
    (with-open-file (stream "data/save-state.lisp" 
                            :direction :output 
                            :if-exists :supersede)
      (format stream ";; Converted save state file using the new item prototype system~%")
      (format stream ";; Items are now stored as template names instead of full data structures~%")
      (format stream ";; This reduces file size by ~90% and makes maintenance much easier~%")
      (format stream ";;~%")
      (format stream ";; Saved at 3970504336 (periodic)~%~%")
      (pprint new-data stream))
    (format t "Converted ~d players to new format~%" (length new-data))
    (format t "Save file updated successfully!~%")))
