;;;; Data Model

(defclass base:blob ()
  (media-type
   content-encoding
   body))

(defclass base:tree ()
  (dentries))

(defclass base:dentry ()
  (name
   id))

(defclass base:commit ()
  (tree-id))

(defclass base:ref ()
  (commit-id))

(defclass base:repo ()
  (name
   heads))

(defun base:make-repo ((name string))
  (make base:repo :name name :heads (list)))

(defmethod show-object ((r base:repo))
  (.name r))

;;;; API

(defun base:list-repos (-> list)
  "Returns locally stored repositories."
  (list))

;;;; User Interface

(defclass base:tracker-mode (mode))

(defun base:make-tracker-mode (-> base:tracker-mode)
  (make base:tracker-mode))

(defmethod mode-init-window ((m base:tracker-mode) (w window))
  (set-window-title w "Content Tracker")
  (dom-append-child
   (window-element w)
   (<div>
    (<div> (<strong> "Repositories"))
    (base:markup-repos))))

(defun base:markup-repos (-> native)
  (let ((repos (base:list-repos)))
    (if (empty? repos)
        (<em> "none")
        (apply \<ul> (map (lambda (repo)
                            (<li> (markup repo)))
                          repos)))))

(defdynamic base:tracker-window (make-window (dynamic window-manager)
                                             nil
                                             :mode (base:make-tracker-mode)))

