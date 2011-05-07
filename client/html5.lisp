;;;; Local Storage

;;; Thanks to Mark Pilgrim for docs
;;; http://diveintohtml5.org/storage.html

(defclass local-storage-error (error))

(defun local-storage-supported? (-> boolean)
  "Returns true if local storage is supported, false otherwise."
  (native-body #{
    try {
      return "localStorage" in window && window["localStorage"] !== null;
    } catch (e) {
      return false;
    }
    #}))

(defun local-storage-get-item ((key string) -> object)
  "Returns the local storage data associated with key, or nil."
  (unless (local-storage-supported?)
    (error (make local-storage-error)))
  #{ localStorage.getItem((~key)) #})

(defun local-storage-set-item ((key string) (data string) -> nil)
  "Updates the local storage data associated with key."
  (unless (local-storage-supported?)
    (error (make local-storage-error)))
  #{ localStorage.setItem((~key), (~data)) #}
  nil)

(defun local-storage-remove-item ((key string) -> nil)
  "Deletes the local storage data associated with key."
  (unless (local-storage-supported?)
    (error (make local-storage-error)))
  #{ localStorage.removeItem((~key)) #}
  nil)

(defun local-storage-length (-> small-integer)
  "Returns the number of local storage items."
  (unless (local-storage-supported?)
    (error (make local-storage-error)))
  #{ localStorage.length #})

(defun local-storage-key ((i small-integer) -> string)
  "Returns the key of the local storage item with the given index."
  (unless (local-storage-supported?)
    (error (make local-storage-error)))
  #{ localStorage.key((~i)) #})

;;;; IndexedDB

(defclass indexed-db-error (error))

(defun indexed-db-supported? (-> boolean)
  "Returns true if IndexedDB is supported, false otherwise."
  (native-body #{
    try {
      return "webkitIndexedDB" in window && window["webkitIndexedDB"] !== null;
    } catch (e) {
      return false;
    }
    #}))

