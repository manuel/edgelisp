(require "local-storage")

;;;; Content Tracker, modelled after Git but much simpler

(defclass base:object)

(defclass base:blob (base:object)
  (body))

(defclass base:tree (base:object)
  (dentries))

(defclass base:dentry ()
  (name
   id))

(defclass base:provider)
(defgeneric base:provider-store (provider base-object -> id))
(defgeneric base:provider-read (provider id -> base-object-or-nil))
(defdynamic base:provider)

(defun base:store ((obj base:object) -> string)
  (base:provider-store (dynamic base:provider) obj))

(defun base:read ((id string) -> object)
  (base:provider-read (dynamic base:provider) id))

;;;; Internal

(defun base:hash ((s string) -> string)
  (sha1 s))

(defun base:make-tree ()
  (make base:tree :dentries (list)))

(defgeneric base:object-content (base-object -> object)
  (:documentation "Translates a base object to JSON."))
(defmethod base:object-content ((b base:blob))
  (.body b))
(defmethod base:object-content ((t base:tree))
  (apply \list (map \base:object-content (.dentries t))))

(defgeneric base:object-type (base-object -> string))
(defmethod base:object-type ((b base:blob)) "blob")
(defmethod base:object-type ((t base:tree)) "tree")

(defgeneric base:init-from-object-content (base-object object-content)
  (:documentation "Reconstructs a base object from its JSON representation."))
(defmethod base:init-from-object-content ((b base:blob) (object-content string))
  (setf (.body b) object-content))
(defmethod base:init-from-object-content ((t base:tree) (object-content list))
  (setf (.dentries t) object-content))

;;;; Local Storage Provider

;;; The local storage provider stores objects very inefficiently using
;;; HTML5 localStorage which is widely supported.

(defclass base:local-provider (base:provider)
  (prefix))

(defun base:make-local-provider ((prefix string))
  (make base:local-provider :prefix prefix))

(defun base:local-object-key ((p base:local-provider) (id string) -> string)
  "Returns local storage key for object ID."
  (string-concat "/provider/" (.prefix p) "/object/" id))

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
                             (error (string-concat "not an object: " type)))))))
          (base:init-from-object-content obj object-content)
          obj))))

(setf (dynamic base:provider) (base:make-local-provider "default"))

(provide "base")
