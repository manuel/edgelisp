;;; Thanks to Mark Pilgrim for docs
;;; http://diveintohtml5.org/storage.html

(defclass local-storage-error (error))

(defun local-storage-supported? ()
  "Returns true if local storage is supported, false otherwise."
  (native-body #{
    try {
      return "localStorage" in window && window["localStorage"] !== null;
    } catch (e) {
      return false;
    }
    #}))

(defun local-storage-get-item ((key string))
  "Returns the local storage data associated with key, or nil."
  (unless (local-storage-supported?)
    (error (make local-storage-error)))
  #{ localStorage.getItem((~key)) #})

(defun local-storage-set-item ((key string) (data string))
  "Updates the local storage data associated with key."
  (unless (local-storage-supported?)
    (error (make local-storage-error)))
  #{ localStorage.setItem((~key), (~value)) #}
  nil)

(defun local-storage-remove-item ((key string))
  "Deletes the local storage data associated with key."
  (unless (local-storage-supported?)
    (error (make local-storage-error)))
  #{ localStorage.removeItem((~key)) #}
  nil)

(defun local-storage-length ()
  "Returns the number of local storage items."
  (unless (local-storage-supported?)
    (error (make local-storage-error)))
  #{ localStorage.length #})

(defun local-storage-key ((i small-integer))
  "Returns the key of the local storage item with the given index."
  (unless (local-storage-supported?)
    (error (make local-storage-error)))
  #{ localStorage.key((~i)) #})

(provide "local-storage")
