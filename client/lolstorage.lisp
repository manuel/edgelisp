;;;; lolstorage: laughably good social networking and content sharing

(defclass lol-wallet ()
  (write-caps)
  (:documentation "A user's wallet contains write capabilities for
  refs controlled by the user, and is typically encrypted with a
  passphrase."))

(defclass lol-cap () ())

(defclass lol-write-cap (lol-cap)
  (write-key
   verify-cap)
  (:documentation "A write capability for mutable content contains the
  write key (derived from the private key) and a verify
  capability (derived from the public key).  From a write capability,
  a read capability can be derived."))

(defclass lol-read-cap (lol-cap)
  (read-key
   verify-cap)
  (:documentation "A read-only capability for mutable content contains
  the read key (derived from the write key) and a verify
  capability (derived from the public key)."))

(defclass lol-mutable-content ()
  (ciphertext
   public-key
   signature
   seqno
   salt)
  (:constructor lol-make-mutable-content ((ciphertext data)
                                          (public-key crypto-public-key)
                                          (signature crypto-signature)
                                          (seqno integer)
                                          (salt salt))))

(defclass lol-immutable-read-cap (lol-cap)
  (encryption-key
   verify-cap)
  (:constructor lol-make-immutable-read-cap ((encryption-key symmetric-key)
                                             (verify-cap hash)))
  (:documentation "A read-only capability for immutable content
  contains the symmetric encryption key and a verify capability."))


(defclass lol-content () ())

(defclass lol-commit (lol-content)
  (parent
   tree
   message))

(defclass lol-tree (lol-content)
  (dentries))

(defclass lol-dentry ()
  (name
   cap))

(defclass lol-blob (lol-content)
  (body))

;;;; API

(defun lol-make-immutable ((data string) -> lol-immutable-read-cap)
  (let* ((encryption-key (lol-make-encryption-key))
         (ciphertext     (lol-encrypt data encryption-key))
         (verify-cap     (lol-hash ciphertext)))
    (lol-write-immutable verify-cap ciphertext)
    (lol-make-immutable-read-cap encryption-key verify-cap)))

(defun lol-make-mutable ((data string) -> lol-write-cap)
  (lol-write (lol-make-private-key) data))

(defun lol-put ((wcap lol-write-cap) (data string))
  (lol-write (lol-write-key-to-private-key (.write-key wcap)) data))

(defgeneric lol-get (cap -> data))

(defmethod lol-get ((wcap lol-write-cap) -> string)
  (lol-get (lol-write-cap-to-read-cap wcap)))

(defmethod lol-get ((rcap lol-read-cap) -> string)
  (let* ((content (lol-read-mutable (.verify-cap rcap)))
         (public-key (.public-key content))
         (signature (.signature content))
         (salt (.salt content)))
    (assert (= (lol-hash public-key) (.verify-cap rcap)))
    (lol-check-signature (.ciphertext content) public-key signature)
    (let ((encryption-key (lol-hash (lol-concat read-key salt))))
      (lol-decrypt ciphertext encryption-key))))

(defmethod lol-get ((icap lol-immutable-read-cap) -> string)
  (lol-decrypt (lol-read-immutable (.verify-cap icap)) (.encryption-key icap)))

;;;; Internal

(defun lol-write ((private-key private-key) (data string) -> lol-write-cap)
  (let* ((public-key     (lol-public-key private-key))
         (verify-cap     (lol-hash public-key))
         (write-key      (lol-hash private-key))
         (read-key       (lol-hash write-key))
         (salt           (lol-make-salt)) ; fixme
         (encryption-key (lol-hash (lol-concat read-key salt)))
         (ciphertext     (lol-encrypt data encryption-key))
         (signature      (lol-sign ciphertext private-key))
         (seqno          (lol-next-seqno read-key))
         (content        (lol-make-mutable-content ciphertext public-key signature seqno salt)))
    (lol-write-mutable verify-cap content)
    (lol-make-write-cap write-key verify-cap)))

