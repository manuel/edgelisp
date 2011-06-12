(require "local-storage")

;;;; Content Tracker, modelled after Git but much simpler

;;; A base stores blobs and trees.  The ID of a blob or tree is its
;;; SHA1 hash.
;;; 
;;; A blob is stored as ["blob", "... contents here ..."].
;;;
;;; A tree is stored as ["tree", ... dentries ...], where dentries is
;;; a list of pairs of the form ["name", "cafe8329827..."].
;;;
;;; A base has a root pointing to the ID of a tree.

(defclass base-object)

(defclass base-blob (base-object)
  (body))

(defclass base-tree (base-object)
  (dentries))

(defclass base-dentry ()
  (name
   id))

(defun base-make-blob ((body string) -> base-blob)
  (make base-blob :body body))

(defun base-make-tree ((dentries list) -> base-tree)
  (make base-tree :dentries dentries))

(defun base-make-dentry ((name object) (id string) -> base-dentry)
  (make base-dentry :name name :id id))

(defun base-store ((obj base-object) -> string)
  (base-provider-store (dynamic base-provider) obj))

(defun base-read ((id string) -> option)
  (base-provider-read (dynamic base-provider) id))

(defun base-root (-> base-tree)
  (base-provider-root (dynamic base-provider)))

(defun base-set-root ((id string))
  (base-provider-set-root (dynamic base-provider) id))

(defun base-object-id ((obj base-object) -> string)
  (base-hash (to-json (base-object-canonical-content obj))))

;;;; Internal

(defmethod show-object ((obj base-object) -> string)
  (show-object (base-object-content obj)))

(defclass base-provider)
(defgeneric base-provider-store (provider base-object -> id))
(defgeneric base-provider-read (provider id -> base-object-option))
(defgeneric base-provider-root (provider -> base-tree))
(defgeneric base-provider-set-root (provider id))
(defdynamic base-provider)

(defun base-hash ((s string) -> string)
  (sha1 s))

(defgeneric base-object-type (base-object -> string))
(defmethod base-object-type ((b base-blob)) "blob")
(defmethod base-object-type ((t base-tree)) "tree")

(defun base-object-canonical-content ((obj base-object) -> list)
  (list (base-object-type obj) (base-object-content obj)))

(defgeneric base-object-content (object -> object))
(defmethod base-object-content ((b base-blob) -> string)
  (.body b))
(defmethod base-object-content ((t base-tree) -> list)
  (apply \list (map \base-object-content (.dentries t))))
(defmethod base-object-content ((de base-dentry) -> list)
  (list (.name de) (.id de)))

(defgeneric base-init-from-object-content (base-object object-content))
(defmethod base-init-from-object-content ((b base-blob) (object-content string))
  (setf (.body b) object-content)
  b)
(defmethod base-init-from-object-content ((t base-tree) (dentries-content list))
  (setf (.dentries t)
        (map (lambda (dentry-content)
               (base-init-from-object-content (make base-dentry) dentry-content))
             dentries-content))
  t)
(defmethod base-init-from-object-content ((de base-dentry) (name-id list))
  (setf (.name de) (elt name-id 0))
  (setf (.id de) (elt name-id 1))
  de)

;;;; Local Storage Provider

;;; The local storage provider stores objects very inefficiently using
;;; HTML5 localStorage which is widely supported.

(defclass base-local-provider (base-provider)
  (prefix))

(defun base-make-local-provider ((prefix string))
  (make base-local-provider :prefix prefix))

(defun base-local-object-key ((p base-local-provider) (id string) -> string)
  "Returns local storage key for object ID."
  (string-concat "/provider/" (.prefix p) "/object/" id))

(defmethod base-provider-store ((p base-local-provider) (obj base-object) -> string)
  "Stores object and returns its ID."
  (let* ((content (base-object-canonical-content obj))
         (json-content (to-json content))
         (id (base-hash json-content))
         (key (base-local-object-key p id)))
    (local-storage-set-item (base-local-object-key p id) json-content)
    id))

(defmethod base-provider-read ((p base-local-provider) (id string) -> option)
  (let* ((key (base-local-object-key p id)))
    (if-option (item (local-storage-get-item key))
      (let* ((content (from-json item))
             (type (elt content 0))
             (object-content (elt content 1))
             (obj (make-instance (ecase type
                                        (("blob") (class base-blob))
                                        (("tree") (class base-tree))))))
        (some (base-init-from-object-content obj object-content)))
      none)))

(defun base-local-provider-root-key ((p base-local-provider))
  (string-concat "/provider/" (.prefix p) "/root"))

(defmethod base-provider-root ((p base-local-provider) -> base-tree)
  (if-option (root-id (local-storage-get-item (base-local-provider-root-key p)))
    (if-option (root (base-provider-read p root-id))
      root
      (error "Cannot read root"))
    (base-make-tree (list))))

(defmethod base-provider-set-root ((p base-local-provider) (id string))
  (local-storage-set-item (base-local-provider-root-key p) id))

(setf (dynamic base-provider) (base-make-local-provider "default"))

(provide "base")
