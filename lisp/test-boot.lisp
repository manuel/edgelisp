;;; defined?

(assert (not (defined? test-boot:the-undefined-variable)))
(assert (not (defined? (identifier test-boot:the-undefined-identifier testns))))
(assert (defined? \show))
(assert (defined? (dynamic handler-frame)))

;;; defparameter

(defparameter test-boot:var1 "foo")
(assert (defined? test-boot:var1))
(assert (= test-boot:var1 "foo"))

(defparameter (identifier test-boot:var2 testns) "bar")
(assert (not (defined? test-boot:var2)))
(assert (defined? (identifier test-boot:var2 testns)))
(assert (= (identifier test-boot:var2 testns) "bar"))

;;; defsyntax

(defsyntax test-boot:defsyntax-test 
  (lambda ((form form)) (compound-elt form 1)))

(let ((foo #`foo))
  (assert (eq (macroexpand-1 #`(test-boot:defsyntax-test ,foo))
              foo)))

;;; eval-when-compile

(eval-when-compile (defvar test-boot:compile-var "quux"))
(defmacro test-boot:eval-when-compile-test ()
  (assert (= test-boot:compile-var "quux"))
  #'nil)
(test-boot:eval-when-compile-test)

;;; eval-and-compile

(eval-and-compile (defvar test-boot:compile-var2 "quux"))
(assert (= test-boot:compile-var2 "quux"))
(defmacro test-boot:eval-and-compile-test ()
  (assert (= test-boot:compile-var2 "quux"))
  #'nil)
(test-boot:eval-and-compile-test)

;;; fdefined?

(assert (not (fdefined? test-boot:just-a-random-function)))
(assert (fdefined? show))

;;; funcall

(assert (= (funcall (lambda () 12)) 12))
(assert (= (funcall (lambda (x) x) 12) 12))

(assert (= (funcall (lambda (&key k) k) :k 1) 1))
(assert (= (funcall (lambda (&key k) k)) nil))
(assert (= (funcall (lambda (a &key k) k) 10 :k 1) 1))
(assert (= (funcall (lambda (a &key k) k) 10) nil))
(assert (= (funcall (lambda (a &key k) a) 10 :k 1) 10))
(assert (= (funcall (lambda (a &key k) a) 10) 10))

(assert (= (funcall (lambda (&key k j) k) :k 1) 1))
(assert (= (funcall (lambda (&key k j) j) :k 1) nil))
(assert (= (funcall (lambda (&key k j) k)) nil))
(assert (= (funcall (lambda (&key k j) j)) nil))
(assert (= (funcall (lambda (a &key k j) j) 10 :k 1) nil))
(assert (= (funcall (lambda (a &key k j) j) 10 :j 1) 1))
(assert (= (funcall (lambda (a &key k j) j) 10) nil))
(assert (= (funcall (lambda (a &key k j) a) 10 :k 1) 10))
(assert (= (funcall (lambda (a &key k j) a) 10 :j 1) 10))
(assert (= (funcall (lambda (a &key k j) a) 10) 10))

(assert (= (funcall (lambda (&key (k 3) (j 4)) k) :k 1) 1))
(assert (= (funcall (lambda (&key (k 3) (j 4)) j) :k 1) 4))
(assert (= (funcall (lambda (&key (k 3) (j 4)) k)) 3))
(assert (= (funcall (lambda (&key (k 3) (j 4)) j)) 4))
(assert (= (funcall (lambda (a &key (k 3) (j 4)) j) 10 :k 1) 4))
(assert (= (funcall (lambda (a &key (k 3) (j 4)) j) 10 :j 1) 1))
(assert (= (funcall (lambda (a &key (k 3) (j 4)) j) 10) 4))
(assert (= (funcall (lambda (a &key (k 3) (j 4)) a) 10 :k 1) 10))
(assert (= (funcall (lambda (a &key (k 3) (j 4)) a) 10 :j 1) 10))
(assert (= (funcall (lambda (a &key (k 3) (j 4)) a) 10) 10))

(assert (= (funcall (lambda (&key (k 3) (j k)) j) :k 1) 1))
(assert (= (funcall (lambda (&key (k 3) (j k)) j)) 3))
(assert (= (funcall (lambda (a &key (k 3) (j k)) j) 10 :k 1) 1))
(assert (= (funcall (lambda (a &key (k 3) (j k)) j) 10 :j 1) 1))
(assert (= (funcall (lambda (a &key (k 3) (j k)) j) 10) 3))

;(assert (= (funcall (lambda (&key k (j "foo") &all-keys keys) (get keys "k")) :k 1) 1))
;(assert (= (funcall (lambda (&key (k 3) (j k) &all-keys keys) (get keys "j"))) nil))

;;; Hygiene

(defmacro test-boot:swap (a b)
  #`(let ((tmp ,b))
      (setq ,b ,a)
      (setq ,a tmp)))

(let ((x 1) (tmp 2))
  (test-boot:swap x tmp)
  (assert (and (= x 2) (= tmp 1)))
  (test-boot:swap tmp x)
  (assert (and (= x 1) (= tmp 2))))

(provide "test-boot")
