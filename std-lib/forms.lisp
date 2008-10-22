;;; "Natural forms", for use by macros.

(defclass <symbol-form> name)

(defun new-symbol-form (jsstr)
  (<symbol-form .name jsstr>))

(def denaturalize (<symbol-form .name>)
  {% { formt: "symbol", name: ~(name) } %})

(defclass <string-form> s)

(defun new-string-form (jsstr)
  (<string-form .s jsstr>))

(def denaturalize (<string-form .s>)
  {% { formt: "string", s: ~(s) } %})

(defclass <compound-form> elts)

(defun new-compound-form (jselts)
  (<compound-form .elts (<array .elts jselts>>)))

(def denaturalize (<compound-form .elts>)
  {% { formt: "compound", elts: ~((.peer (map #'denaturalize elts))) %})

(def iterator (<compound-form .elts>)
  (iterator elts))

(def slice (<compound-form .elts>)
  (<compound-form .elts (slice elts)))

(def [] (<compound-form .elts> i)
  ([] elts i))

(def type-for-copy (<compound-form>)
  <compound-form>)

(defun compound-forms-append (forms)
  (let ((res {% [] %}))
    (for-each (lambda (<form .elts>)
                {% ~(res).concat(~((.peer elts))) %})
              forms)
    (new-compound-form res)))
