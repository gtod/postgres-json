;;;; Fragments related to "real world" work on bookings

;; *bookings* should be result of cusoon: (run-event-loop ()
;;                                          (run-sheet-bookings))

(defun create-booking-model ()
  (create-model 'booking (make-model-parameters 'booking :key 'id :key-type 'uuid)))

(defun populate-bookings ()
  (with-model-transaction ()
    (loop for booking across *bookings*
          do (insert 'booking booking :use-key (gethash "id" booking) :stash-key nil))))

