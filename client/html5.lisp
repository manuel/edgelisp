;;;; Local Storage

;;; Thanks to Mark Pilgrim for docs
;;; http://diveintomarkup5.org/storage.html

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

(defmacro define-local-storage-function (name sig &rest body)
  #`(defun ,name ,sig
      (unless (local-storage-supported?)
        (error (make local-storage-error)))
      ,@body))

(define-local-storage-function local-storage-get-item ((key string) -> option)
  "Returns the local storage data associated with key as an option."
  (nil-to-option #{ localStorage.getItem((~key)) #}))

(define-local-storage-function local-storage-set-item ((key string) (data string) -> nil)
  "Updates the local storage data associated with key."
  #{ localStorage.setItem((~key), (~data)) #}
  nil)

(define-local-storage-function local-storage-remove-item ((key string) -> nil)
  "Deletes the local storage data associated with key."
  #{ localStorage.removeItem((~key)) #}
  nil)

(define-local-storage-function local-storage-length (-> small-integer)
  "Returns the number of local storage items."
  #{ localStorage.length #})

(define-local-storage-function local-storage-key ((i small-integer) -> string)
  "Returns the key of the local storage item with the given index."
  #{ localStorage.key((~i)) #})

(provide "local-storage")

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

