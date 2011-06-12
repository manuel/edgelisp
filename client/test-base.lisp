(let* ((blob (base-make-blob "Hello world!"))
       (tree (base-make-tree
              (list (base-make-dentry "hello.txt" (base-object-id blob)))))
       (blob-id (base-store blob))
       (tree-id (base-store tree)))
  (if-option (stored-blob (base-read blob-id))
    (assert (= (base-object-content stored-blob) "Hello world!"))
    (error "Cannot read blob")))
