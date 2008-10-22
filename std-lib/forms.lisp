;;; Reified code, for use by macros.

(defclass <compound-form> elts)

(def iterator (<compound-form .elts>)
  (iterator elts))

(def slice (<compound-form .elts>)
  (<compound-form .elts (slice elts)))

(def [] (<compound-form .elts> i)
  ([] elts i))

(def type-for-copy (<compound-form>)
  <compound-form>)

(defun new-compound-form (jselts)
  (<compound-form .elts (<array .elts jselts>>)))


(defclass <symbol-form> name)

(defun new-symbol-form (jsstr)
  (<symbol-form .name jsstr>))


(defclass <string-form> s)

(defun new-string-form (jsstr)
  (<string-form .s jsstr>))


(defun compound-forms-append (forms)
  (let ((res {% [] %}))
    (for-each (lambda (<form .elts>)
                {% lispVar("res").concat(~(elts)) %}))
    (new-compound-form res)))
