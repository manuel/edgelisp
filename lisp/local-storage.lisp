(defclass local-storage-error (error))

(defun local-storage-supported? ()
  (native-body #{
    try {
      return "localStorage" in window && window["localStorage"] !== null;
    } catch (e) {
      return false;
    }
    #}))

(defun local-storage-get-item ((key string))
  (unless (local-storage-supported?)
    (error (make local-storage-error)))
  #{ localStorage.getItem((~key)) #})

(defun local-storage-set-item ((key string) (value string))
  (unless (local-storage-supported?)
    (error (make local-storage-error)))
  #{ localStorage.setItem((~key), (~value)) #})

(provide "local-storage")
