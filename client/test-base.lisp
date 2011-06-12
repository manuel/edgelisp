(defvar test-blob (base-make-blob "Hello content-centric world!"))
(defvar test-tree (base-make-tree
                   (list (base-make-dentry "hello.txt" (base-object-id test-blob)))))
(defvar test-blob-id (base-store test-blob))
(defvar test-tree-id (base-store test-tree))

(base-set-root test-tree-id)

(if-option (stored-blob (base-read test-blob-id))
  (assert (= (base-object-content stored-blob) "Hello content-centric world!"))
  (error "Cannot read blob"))
