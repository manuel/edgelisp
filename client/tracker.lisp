;;;;; Git-clone for the browser
;;;;; by Manuel Simoni, 2011-05-17

(defclass tr:repo () ()
  (:documentation "A repository contains references and objects."))

(defclass tr:object () ()
  (:documentation "Every object has an unique ID, computed by hashing
  the object's contents.  Objects may be blobs, trees, or commits."))
    
(defclass tr:blob (tr:object)
  (data)
  (:documentation "A blob is an untyped sequence of bytes."))

(defclass tr:tree (tr:object)
  (dentries)
  (:documentation "A tree is a list of dentries, usually the contents
  of a directory."))

(defclass tr:dentry ()
  (name
   id)
  (:documentation "A dentry is a single named item in a directory."))

(defclass tr:commit (tr:object)
  (tree
   parents)
  (:documentation "A commit records the state of a tree in time and
  previous states."))

(defun tr:blob ((data string))
  (make tr:blob :data data))

(defun tr:tree (&rest dentries)
  (make tr:tree :dentries dentries))

(defun tr:dentry ((name string) (id string))
  (make tr:dentry :name name :id id))

(defun tr:commit ((tree string) &optional (parents (list)))
  (make tr:commit :tree tree :parents parents))

(defgeneric tr:ref (repo ref-name -> ref-value)
  (:documentation "Reads the value of a reference, usually a commit ID."))

(defgeneric tr:ref-setter (repo ref-name ref-value)
  (:documentation "Updates the value of a reference."))

;;;; Web Storage Repository

(defclass tr:local-repo (tr:repo)
  (name)
  (:documentation "A repository that stores its data in Web Storage."))

(defun tr:make-local-repo ((name string))
  (make tr:local-repo :name name))

(defmethod tr:ref ((repo tr:local-repo) (name string) -> string))

(defmethod tr:ref-setter ((repo tr:local-repo) (name string) (value string)))

