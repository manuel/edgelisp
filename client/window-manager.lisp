;;;; Tiling window manager

(defclass window-manager)
(defclass window ()
  (buffer
   mode))
(defgeneric make-window (wm -> window))
(defgeneric set-window-content (window content))
(defgeneric set-window-position (window x y))
(defgeneric set-window-size (window w h))
(defgeneric window-element (window -> native))

(defclass jwim-window-manager (window-manager)
  (native
   windows))
(defmethod show-object ((wm jwim-window-manager)) "")

(defclass jwim-window (window)
  (native))
(defmethod show-object ((wm jwim-window)) "")

(defun make-jwim-window-manager (-> jwim-window-manager)
  (make jwim-window-manager
        :native #{ new jwim.Manager() #}
        :windows (list)))

(defmethod make-window ((wm jwim-window-manager)
                        &key (buffer (make-buffer)) (mode (make-fundamental-mode))
                        -> jwim-window)
  (let ((w (make jwim-window
                 :native #{ ~(.native wm).createWindow() #}
                 :buffer buffer
                 :mode mode)))
    (jwim-add-window wm w)
    w))

(defun jwim-add-window ((wm jwim-window-manager) (w jwim-window))
  (add (.windows wm) w))

(defmethod set-window-content ((w jwim-window) (content string))
  #{ ~(.native w).setContent(~content), null #})

(defmethod set-window-position ((w jwim-window) (x small-integer) (y small-integer))
  #{ ~(.native w).setPosition(~x, ~y), null #})

(defmethod set-window-size ((w jwim-window) (width small-integer) (height small-integer))
  #{ ~(.native w).setSize(~width, ~height), null #})

(defmethod window-element ((w jwim-window) -> native)
  #{ ~(.native w).getElement() #})

;;; Buffers

(defclass buffer ()
  (items))
(defun make-buffer (-> buffer)
  (make buffer :items (list)))

(defclass mode)

(defclass fundamental-mode (mode))
(defun make-fundamental-mode (-> fundamental-mode)
  (make fundamental-mode))

(defclass repl-mode (mode))
(defun make-repl-mode (-> repl-mode)
  (make repl-mode))

(defvar *window-manager* (make-jwim-window-manager))
(defvar *top-window* (make-window *window-manager*))
(set-window-content *top-window* "Hello")
(set-window-position *top-window* 0 0)
(set-window-size *top-window* (window-inner-width) (window-inner-height))

