;;;; Tiling window manager

(defclass window-manager)
(defclass window)
(defgeneric make-window (wm -> window))
(defgeneric set-window-content (window content))

(defclass jwim-window-manager (window-manager)
  (native))
(defmethod show-object ((wm jwim-window-manager)) "")

(defclass jwim-window (window)
  (native))
(defmethod show-object ((wm jwim-window)) "")

(defun make-jwim-window-manager (-> jwim-window-manager)
  (make jwim-window-manager :native #{ new jwim.Manager() #}))

(defmethod make-window ((wm jwim-window-manager) -> jwim-window)
  (make jwim-window :native #{ ~(.native wm).createWindow() #}))

(defmethod set-window-content ((w jwim-window) (content string))
  #{ ~(.native w).setContent(~content), null #})

(provide "window-manager")
