(defclass <symbol-form> name)

(defun new-symbol-form (name)
  (<symbol-form .name name>))

(defclass <string-form> s)

(defun new-string-form (s)
  (<string-form .s s>))

(defclass <compound-form> elts)

(defun new-compound-form (elts)
  (<compound-form .elts (<array .peer elts>)>))

(def iterator (<compound-form .elts>)
  (iterator elts))

(def slice (<compound-form .elts> start) 
  (<compound-form .elts (slice elts start)>))

(def [] (<compound-form .elts> i) 
  ([] elts i))

(def type-for-copy (<compound-form>) 
  <compound-form>)

(def []-setter (<compound-form .elts> i v) 
  ([]-setter elts i v))

(def empty-clone (<compound-form .elts>) 
  (<compound-form .elts (empty-clone elts)>))
