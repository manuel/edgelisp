;; -*- LISP -*-

;; number iterators

(defparameter ni-ct 0)
(each (lambda (num) (inc ni-ct)) 
      500)
(assert-eql ni-ct 500)

;; alien

(assert-eql {% 7 * 3 %} 21)
(defvar lisp-x 2)
(assert-eql 12 {% ~lisp-x + 10 %})
(assert-eql 6 {% ~(+ lisp-x {% 2 * ~lisp-x %}) %})
(assert-eql "%" {% "%" %})

;; map

(defvar l1 (list 1 2 3))
(defvar l2 (list 1 2 3 4))
(defvar res (map #'+ l1 l2))
(assert-eql (elt res 0) 2)
(assert-eql (elt res 1) 4)
(assert-eql (elt res 2) 6)
(assert-eql (len res) 3)

(defvar list (list 1 2 3))
(assert-eql (len list) 3)
(defvar list-ct 0)
(each (lambda (elt) (inc list-ct elt))
      list)
(assert-eql list-ct 6)

(assert-eql (progn 1 2 3) 3)
(assert-eql (progn 1) 1)
(assert-eql (progn) null)

(assert-eql (funcall (lambda ())) null)
(assert-eql (funcall (lambda () 1)) 1)
(assert-eql (funcall (lambda (x) x) 1) 1)
(assert-eql (funcall (lambda (x &opt (y x)) y) 1) 1)

(defun test-x () 117)
(assert-eql (test-x) 117)

(defun test-y (y) y)
(assert-eql (test-y 133) 133)

;; Exceptions

(defclass (<my-error> <error>))
(defclass (<your-error> <error>))

(defvar test-counter 0)
(catch ((<my-error> (lambda (e next-handler)
                      (set test-counter 1))))
  (signal (<my-error>)))
(assert-eql test-counter 1)

(set test-counter 0)
(catch ((<your-error> (lambda (e next-handler)
                        (set test-counter 2))))
  (catch ((<my-error> (lambda (e next-handler)
                        (set test-counter 1))))
    (signal (<your-error>))))
(assert-eql test-counter 2)

(set test-counter 0)
(catch ((<your-error> (lambda (e next-handler)
                        (set test-counter 2)))
        (<my-error> (lambda (e next-handler)
                      (set test-counter 1))))
  (signal (<your-error>)))
(assert-eql test-counter 2)

(set test-counter 0)
(catch ((<your-error> (lambda (e next-handler)
                        (set test-counter 2)))
        (<my-error> (lambda (e next-handler)
                      (set test-counter 1))))
  (signal (<my-error>)))
(assert-eql test-counter 1)

(set test-counter 0)
(catch ((<my-error> (lambda (e next-handler)
                      (set test-counter 1))))
  (catch ((<your-error> (lambda (e next-handler)
                          (set test-counter 2)))
          (<my-error> (lambda (e next-handler)
                        (funcall next-handler))))
    (signal (<my-error>))))
(assert-eql test-counter 1)

(set test-counter 0)
(catch ((<my-error> (lambda (e next-handler)
                      (assert-eql test-counter 3))))
  (catch ((<your-error> (lambda (e next-handler)
                          (set test-counter 2)))
          (<my-error> (lambda (e next-handler)
                        (set test-counter 3)
                        (funcall next-handler))))
    (signal (<my-error>))))
(assert-eql test-counter 3)


;; #' syntax

(defun foo () 12)
(assert #'foo)
(assert-eql #'foo (function foo))
(assert-eql (funcall #'foo) 12)

;; Escape functions

(assert-eql null (block x 13 (return-from x) 12))
(assert-eql 1 (block x (return-from x 1)))
(assert-eql 1 (block x (block y (return-from x 1))))
(assert-eql 2 (block x (block y 2)))
(assert-eql 3 (block x (block y 2) 3))
(assert-eql 3 (block x (block y (return-from y 4)) 3))
(assert-eql 4 (block x (block y (return-from y 4))))

(defun jumper (exit) 
  (return-from exit 33))
(assert-eql 33 (block outer (jumper outer)))

;; Unwind protect

(defclass (<another-error> <error>))

(defvar unwind-ct 0)
(catch ((<another-error> (lambda (e next-handler))))
  (unwind-protect (signal (<another-error>))
    (set unwind-ct 1)))
(assert-eql unwind-ct 1)

(set unwind-ct 0)
(assert-eql 2 (unwind-protect 2
               (set unwind-ct 1)))
(assert-eql unwind-ct 1)

(set unwind-ct 0)
(assert-eql 12 (block exit
                (unwind-protect (return-from exit 12)
                  (set unwind-ct 1))))
(assert-eql unwind-ct 1)

;; Slots

(defclass <foo>)
(defvar foo (<foo>))
(assert-eql (slot foo "slot1") null)
(assert-eql (slot foo "slot2") null)
(set-slot foo "slot1" 1)
(assert-eql (slot foo "slot1") 1)
(assert-eql (slot foo "slot2") null)
(set-slot foo "slot1" null)
(assert-eql (slot foo "slot1") null)
(assert-eql (slot foo "slot2") null)

(assert (has-slot foo "slot1"))
(assert (not (has-slot foo "slot2")))

(defclass <z> (a b c))
(defvar z (<z>))
(assert-eql (.a z) null)
(assert-eql (.b z) null)
(assert-eql (.c z) null)
(set (.a z) 1)
(set (.b z) 2)
(set (.c z) 3)
(assert-eql (.a z) 1)
(assert-eql (.b z) 2)
(assert-eql (.c z) 3)

;; Methods

(defclass <quux>)
(set-method <quux> "m33" (lambda ((quux <quux>)) 33))
(set-method <quux> "m44" (lambda ((quux <quux>)) 44))
(defvar quux (<quux>))
(assert-eql (funcall (get-method quux "m33") quux) 33)
(assert-eql (funcall (get-method quux "m44") quux) 44)
(defclass (<sub> <quux>))
(defvar sub (<sub>))
(assert-eql (funcall (get-method sub "m33") sub) 33)
(assert-eql (funcall (get-method sub "m44") sub) 44)

(defmethod m55 ((quux <quux>)) 55)
(assert-eql (m55 quux) 55)
(assert-eql (m55 sub) 55)

(defmethod m66 ((quux <quux>)) 66)
(assert-eql (m66 quux) 66)
(assert-eql (m66 sub) 66)

(defmethod mid ((quux <quux>) x) x)
(assert-eql (mid quux 12) 12)
(assert-eql (mid sub 12) 12)

;; Keyword arguments

(assert-eqll (funcall (lambda (&key k) k) k: 1) 1)
(assert-eql (funcall (lambda (&key k) k)) null)
(assert-eql (funcall (lambda (a &key k) k) 10 k: 1) 1)
(assert-eql (funcall (lambda (a &key k) k) 10) null)
(assert-eql (funcall (lambda (a &key k) a) 10 k: 1) 10)
(assert-eql (funcall (lambda (a &key k) a) 10) 10)

(assert-eql (funcall (lambda (&key k j) k) k: 1) 1)
(assert-eql (funcall (lambda (&key k j) j) k: 1) null)
(assert-eql (funcall (lambda (&key k j) k)) null)
(assert-eql (funcall (lambda (&key k j) j)) null)
(assert-eql (funcall (lambda (a &key k j) j) 10 k: 1) null)
(assert-eql (funcall (lambda (a &key k j) j) 10 j: 1) 1)
(assert-eql (funcall (lambda (a &key k j) j) 10) null)
(assert-eql (funcall (lambda (a &key k j) a) 10 k: 1) 10)
(assert-eql (funcall (lambda (a &key k j) a) 10 j: 1) 10)
(assert-eql (funcall (lambda (a &key k j) a) 10) 10)

(assert-eql (funcall (lambda (&key (k 3) (j 4)) k) k: 1) 1)
(assert-eql (funcall (lambda (&key (k 3) (j 4)) j) k: 1) 4)
(assert-eql (funcall (lambda (&key (k 3) (j 4)) k)) 3)
(assert-eql (funcall (lambda (&key (k 3) (j 4)) j)) 4)
(assert-eql (funcall (lambda (a &key (k 3) (j 4)) j) 10 k: 1) 4)
(assert-eql (funcall (lambda (a &key (k 3) (j 4)) j) 10 j: 1) 1)
(assert-eql (funcall (lambda (a &key (k 3) (j 4)) j) 10) 4)
(assert-eql (funcall (lambda (a &key (k 3) (j 4)) a) 10 k: 1) 10)
(assert-eql (funcall (lambda (a &key (k 3) (j 4)) a) 10 j: 1) 10)
(assert-eql (funcall (lambda (a &key (k 3) (j 4)) a) 10) 10)

(assert-eql (funcall (lambda (&key (k 3) (j k)) j) k: 1) 1)
(assert-eql (funcall (lambda (&key (k 3) (j k)) j)) 3)
(assert-eql (funcall (lambda (a &key (k 3) (j k)) j) 10 k: 1) 1)
(assert-eql (funcall (lambda (a &key (k 3) (j k)) j) 10 j: 1) 1)
(assert-eql (funcall (lambda (a &key (k 3) (j k)) j) 10) 3)

(assert-eql (funcall (lambda (&key (k 3) (j k) &all-keys keys) (get keys "k")) k: 1) 1)
(assert-eql (funcall (lambda (&key (k 3) (j k) &all-keys keys) (get keys "j"))) null)

;; apply

(defvar dict (<string-dict>))
(put dict "foo" 1)
(put dict "bar" 2)
(defun x (&key foo bar) foo)
(defun y (&key foo bar) bar)
(assert-eql (apply #'x (list) dict) 1)
(assert-eql (apply #'y (list) dict) 2)

