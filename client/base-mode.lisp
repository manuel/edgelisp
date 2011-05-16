(require "base")

(defclass base-mode (fundamental-mode))

(defmethod mode-init-window ((m base-mode) (w window))
  (set-window-title w (window-buffer w))
  (mode-refresh-window m w))

(defmethod mode-refresh-window ((m base-mode) (w window))
  (dom-empty (window-element w))
  (let* ((repo (base:open (window-buffer w)))
         (tree-id (.tree-id repo))
         (tree (the base:tree (base:read tree-id))))
        (dom-append-child (window-element w)
                          (if (empty? (.dentries tree))
                              (<p> "Empty")
                              (apply \<ul> (map (lambda (dentry)
                                                  (<li> (.name dentry)))
                                                (.dentries tree)))))))
  
(make-window (dynamic window-manager) "default" :mode (make base-mode))


