(defclass outline ()
  (name))

(defun make-outline ((name string))
  (make outline :name name))

(defmethod show ((o outline))
  (.name o))

(defmethod markup ((o outline))
  (markup "Outline."))

(defclass outline-mode (mode))

(defmethod mode-init-window ((m outline-mode) (w window))
  (set-window-title w (show (window-buffer w)))
  (dom-append-child
   (window-element w)
   (markup (window-buffer w))))

(defdynamic outline-window (make-window (dynamic window-manager)
                                        (make-outline "default-outline")
                                        :mode (make outline-mode)))

