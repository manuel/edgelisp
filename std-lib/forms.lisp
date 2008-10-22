;;; "Natural forms", for use by macros.

(defclass <symbol-form> name)

(defun new-symbol-form (name) (<symbol-form .name name>))

; (def denaturalize (<symbol-form .name>) {% { formt: "symbol", name: ~(name) } %})

(defclass <string-form> s)

(defun new-string-form (s) (<string-form .s s>))

; (def denaturalize (<string-form .s>) {% { formt: "string", s: ~(s) } %})

(defclass <compound-form> elts)

(defun new-compound-form (elts) (<compound-form .elts (<array .peer elts>)>))

; (def denaturalize (<compound-form .elts>)
;  {% { formt: "compound", elts: ~((.peer (map #'denaturalize elts))) } %})

(def iterator (<compound-form .elts>) (iterator elts))

(def slice (<compound-form .elts>) (<compound-form .elts (slice elts)>))

(def [] (<compound-form .elts> i) ([] elts i))

(def type-for-copy (<compound-form>) <compound-form>)
