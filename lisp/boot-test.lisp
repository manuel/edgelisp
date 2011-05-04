;;; defined?

(assert (not (defined? boot-test:the-undefined-variable)))
(assert (not (defined? (identifier boot-test:the-undefined-identifier testns))))
(assert (defined? \show))
(assert (defined? (dynamic handler-frame)))

;;; defparameter

(defparameter boot-test:var1 "foo")
(assert (defined? boot-test:var1))
(assert (= boot-test:var1 "foo"))

(defparameter (identifier boot-test:var2 testns) "bar")
(assert (not (defined? boot-test:var2)))
(assert (defined? (identifier boot-test:var2 testns)))
(assert (= (identifier boot-test:var2 testns) "bar"))

;;; defsyntax

(defsyntax boot-test:defsyntax-test 
  (lambda ((form form)) (compound-elt form 1)))

(let ((foo #`foo))
  (assert (eq (macroexpand-1 #`(boot-test:defsyntax-test ,foo))
              foo)))

;;; eval-when-compile

(eval-when-compile (defvar boot-test:compile-var "quux"))
(defmacro boot-test:eval-when-compile-test ()
  (assert (= boot-test:compile-var "quux"))
  #'nil)
(boot-test:eval-when-compile-test)

;;; eval-and-compile

(eval-and-compile (defvar boot-test:compile-var2 "quux"))
(assert (= boot-test:compile-var2 "quux"))
(defmacro boot-test:eval-and-compile-test ()
  (assert (= boot-test:compile-var2 "quux"))
  #'nil)
(boot-test:eval-and-compile-test)

;;; fdefined?

(assert (not (fdefined? boot-test:just-a-random-function)))
(assert (fdefined? show))

;;; funcall

(assert (= (funcall (lambda () 12)) 12))
(assert (= (funcall (lambda (x) x) 12) 12))

(assert (= (funcall (lambda (&key k) k) :k 1) 1))
(assert (eq (funcall (lambda (&key k) k)) nil))
(assert (= (funcall (lambda (a &key k) k) 10 :k 1) 1))
(assert (eq (funcall (lambda (a &key k) k) 10) nil))
(assert (= (funcall (lambda (a &key k) a) 10 :k 1) 10))
(assert (= (funcall (lambda (a &key k) a) 10) 10))

(assert (= (funcall (lambda (&key k j) k) :k 1) 1))
(assert (eq (funcall (lambda (&key k j) j) :k 1) nil))
(assert (eq (funcall (lambda (&key k j) k)) nil))
(assert (eq (funcall (lambda (&key k j) j)) nil))
(assert (eq (funcall (lambda (a &key k j) j) 10 :k 1) nil))
(assert (= (funcall (lambda (a &key k j) j) 10 :j 1) 1))
(assert (eq (funcall (lambda (a &key k j) j) 10) nil))
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

(provide "boot-test")
