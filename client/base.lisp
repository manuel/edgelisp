;;;; Content Tracker, modelled after Git

(defclass base:object)

(defclass base:blob (base:object)
  (body))

(defclass base:tree (base:object)
  (dentries))

(defun base:make-tree ()
  (make base:tree :dentries (list)))

(defclass base:dentry ()
  (name
   id))

(defclass base:commit (base:object)
  (tree-id))

(defclass base:ref ()
  (commit-id))

(defclass base:repo ()
  (name
   tree-id))

(defun base:make-repo ((name string) (tree-id string) -> base:repo)
  (make base:repo :name name :tree-id tree-id))

(defmethod show-object ((r base:repo) -> string)
  (.name r))

;;;; Storage Providers

(defclass base:provider)

(defgeneric base:open-repo (provider name -> repo))

;;;; Local Storage Provider

;;; The local storage provider stores repositories very inefficiently
;;; using HTML5 localStorage which is widely supported.

(defclass base:local-provider (base:provider)
  (prefix))

(defun base:local-key ((p base:local-provider) (repo-name string) -> string)
  "Returns local storage key for repository name."
  (string-concat (.prefix p) ".repo." repo-name))

(defmethod base:open-repo ((p base:local-provider) (name string) -> base:repo)
  (let* ((key (base:local-key p name))
         (tree-id (local-storage-get-item key)))
    (if (nil? tree-id)
        (base:local-init-repo p name)
        (base:make-repo name tree-id))))

(defun base:local-init-repo ((p base:local-provider) (name string) -> base:repo)
  (let* ((tree (base:make-tree))
         (tree-id (base:store p tree)))
    (base:make-repo name tree-id)))

;;;; API

(defun base:hash ((s string) -> string)
  (sha1 s))

(defun base:store ((p base:local-provider) (obj base:object) -> string)
  "Stores object and returns its ID."
  (let* ((content (list (base:canonical-type obj) (base:canonical-content obj)))
         (json-content (to-json content))
         (id (base:hash json-content)))
    (local-storage-set-item id json-content)
    id))

(defgeneric base:canonical-content (base-object -> object)
  (:documentation "Translates a base object to JSON."))
(defmethod base:canonical-content ((b base:blob))
  (.body b))
(defmethod base:canonical-content ((t base:tree))
  (apply \list (map \base:canonical-content (.dentries t))))
(defmethod base:canonical-content ((c base:commit))
  (.tree-id c))

(defgeneric base:canonical-type (base-object -> string))
(defmethod base:canonical-type ((b base:blob)) "blob")
(defmethod base:canonical-type ((t base:tree)) "tree")
(defmethod base:canonical-type ((c base:commit)) "commit")

(provide "base")
