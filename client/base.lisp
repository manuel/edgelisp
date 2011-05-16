(require "local-storage")

;;;; Content Tracker, modelled after Git

(defclass base:object)

(defclass base:blob (base:object)
  (body))

(defclass base:tree (base:object)
  (dentries))

(defclass base:dentry ()
  (name
   id))

(defclass base:commit (base:object)
  (tree-id))

(defclass base:repo ()
  (name
   tree-id))

(defclass base:provider)
(defgeneric base:provider-open (provider repo-name -> repo))
(defgeneric base:provider-store (provider base-object -> id))
(defgeneric base:provider-read (provider id -> base-object-or-nil))
(defdynamic base:provider)

(defun base:open ((repo-name string) -> base:repo)
  (base:provider-open (dynamic base:provider) repo-name))

(defun base:store ((obj base:object) -> string)
  (base:provider-store (dynamic base:provider) obj))

(defun base:read ((id string) -> object)
  (base:provider-read (dynamic base:provider) id))

;;;; Internal

(defun base:hash ((s string) -> string)
  (sha1 s))

(defun base:make-tree ()
  (make base:tree :dentries (list)))

(defun base:make-repo ((name string) (tree-id string) -> base:repo)
  (make base:repo :name name :tree-id tree-id))

(defmethod show-object ((r base:repo) -> string)
  (.name r))

(defgeneric base:object-content (base-object -> object)
  (:documentation "Translates a base object to JSON."))
(defmethod base:object-content ((b base:blob))
  (.body b))
(defmethod base:object-content ((t base:tree))
  (apply \list (map \base:object-content (.dentries t))))
(defmethod base:object-content ((c base:commit))
  (.tree-id c))

(defgeneric base:object-type (base-object -> string))
(defmethod base:object-type ((b base:blob)) "blob")
(defmethod base:object-type ((t base:tree)) "tree")
(defmethod base:object-type ((c base:commit)) "commit")

(defgeneric base:init-from-object-content (base-object object-content)
  (:documentation "Reconstructs a base object from its JSON representation."))
(defmethod base:init-from-object-content ((b base:blob) (object-content string))
  (setf (.body b) object-content))
(defmethod base:init-from-object-content ((t base:tree) (object-content list))
  (setf (.dentries t) object-content))
(defmethod base:init-from-object-content ((c base:commit) (object-content string))
  (setf (.tree-id c) object-content))

;;;; Local Storage Provider

;;; The local storage provider stores repositories very inefficiently
;;; using HTML5 localStorage which is widely supported.

(defclass base:local-provider (base:provider)
  (prefix))

(defun base:make-local-provider ((prefix string))
  (make base:local-provider :prefix prefix))

(defun base:local-repo-key ((p base:local-provider) (repo-name string) -> string)
  "Returns local storage key for repository name."
  (string-concat "/provider/" (.prefix p) "/repo/" repo-name))

(defun base:local-object-key ((p base:local-provider) (id string) -> string)
  "Returns local storage key for object ID."
  (string-concat "/provider/" (.prefix p) "/object/" id))

(defmethod base:provider-open ((p base:local-provider) (name string) -> base:repo)
  (let* ((key (base:local-repo-key p name))
         (tree-id (local-storage-get-item key)))
    (if (nil? tree-id)
        (base:local-init-repo p name)
        (base:make-repo name tree-id))))

(defun base:local-init-repo ((p base:local-provider) (name string) -> base:repo)
  (let* ((tree (base:make-tree))
         (tree-id (base:provider-store p tree)))
    (base:make-repo name tree-id)))

(defmethod base:provider-store ((p base:local-provider) (obj base:object) -> string)
  "Stores object and returns its ID."
  (let* ((content (list (base:object-type obj) (base:object-content obj)))
         (json-content (to-json content))
         (id (base:hash json-content))
         (key (base:local-object-key p id)))
    (local-storage-set-item (base:local-object-key p id) json-content)
    id))

(defmethod base:provider-read ((p base:local-provider) (id string) -> object)
  (let* ((key (base:local-object-key p id))
         (content (from-json (local-storage-get-item key))))
    (if (nil? content)
        nil
        (let* ((type (elt content 0))
               (object-content (elt content 1))
               (obj (make-instance
                     (if (= type "blob") (class base:blob) ; HECK, need CASE!
                         (if (= type "tree") (class base:tree)
                             (if (= type "commit") (class base:commit)
                                 (error (string-concat "not an object: " type))))))))
          (base:init-from-object-content obj object-content)
          obj))))

(setf (dynamic base:provider) (base:make-local-provider "default"))

(provide "base")
