;;;; Window Manager

(defclass window-manager)
(defclass window ()
  (buffer
   mode))
(defgeneric make-window (wm -> window))
(defgeneric set-window-content (window content))
(defgeneric set-window-title (window title))
(defgeneric set-window-position (window x y))
(defgeneric set-window-size (window w h))
(defgeneric window-element (window -> native))
(defgeneric window-buffer (window -> buffer))
(defgeneric window-find (window id -> native))

(defclass mode)
(defgeneric mode-init-window (mode window))
(defmethod mode-init-window ((m mode) (w window)))

(defclass fundamental-mode (mode))
(defun make-fundamental-mode (-> fundamental-mode)
  (make fundamental-mode))

(defclass buffer ()
  (entries))
(defun make-buffer ()
  (make buffer :entries (list)))

;;; Window manager implementation based on JWIM

(defclass jwim-window-manager (window-manager)
  (native
   windows))
(defmethod show-object ((wm jwim-window-manager)) "")

(defclass jwim-window (window)
  (native))
(defmethod show-object ((wm jwim-window)) "")

(defun make-jwim-window-manager (-> jwim-window-manager)
  (make jwim-window-manager
        :native
        #{ new jwim.Manager({showMaximizer:false,
                            showClose:false,
                            showResizer:false,
                            defaultState:"maximized"
                            })
        #}
        :windows (list)))

(defmethod make-window ((wm jwim-window-manager)
                        &key 
                        (mode (make-fundamental-mode))
                        (buffer (make-buffer))
                        (container (dom-document-body))
                        -> jwim-window)
  (let ((w (make jwim-window
                 :native #{ ~(.native wm).createWindow() #}
                 :mode mode
                 :buffer buffer)))
    (jwim-add-window wm w)
    (mode-init-window mode w)
    w))

(defun jwim-add-window ((wm jwim-window-manager) (w jwim-window))
  (add (.windows wm) w))

(defmethod set-window-content ((w jwim-window) (content string))
  #{ ~(.native w).setContent(~content), null #})

(defmethod set-window-title ((w jwim-window) (title string))
  #{ ~(.native w).setTitle(~title), null #})

(defmethod set-window-position ((w jwim-window) (x small-integer) (y small-integer))
  #{ ~(.native w).setPosition(~x, ~y), null #})

(defmethod set-window-size ((w jwim-window) (width small-integer) (height small-integer))
  #{ ~(.native w).setSize(~width, ~height), null #})

(defmethod window-element ((w jwim-window) -> native)
  #{ ~(.native w).content #})

(defmethod window-find ((w jwim-window) (id string) -> object)
  #{ ~(.native w).getElement(~id) #})

(defvar *window-manager* (make-jwim-window-manager))
