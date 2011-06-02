(define-native-slot fasl-times "times")

(defun fasl-add-effects ((fasl fasl) (code string) (time string))
  "Adds code to the specified time of the fasl."
  (let ((times (.fasl-times fasl)))
    (if (native-slot-defined? times time)
        (set-native-slot-value times time (string-concat (native-slot-value times time)
                                                         ", "
                                                         code))
        (set-native-slot-value times time code))))

(defun link (&key (time "execute") &rest fasls)
  "Combines the effects (at time TIME) of the given fasls into a single fasl."
  (link-list-of-fasls fasls :time time))

(defun link-list-of-fasls ((fasls list) &key (time "execute"))
  (let ((result (make-fasl)))
    (each (lambda (fasl)
            (let ((times (.fasl-times fasl)))
              (when (native-slot-defined? times time)
                (fasl-add-effects result (native-slot-value times time) time))))
          fasls)
    result))
