; EdgeLisp: A Lisp that compiles to JavaScript.
; Copyright (C) 2008-2011 by Manuel Simoni.
;
; This program is free software: you can redistribute it and/or modify
; it under the terms of the GNU Affero General Public License as
; published by the Free Software Foundation, version 3.
;
; This program is distributed in the hope that it will be useful,
; but WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
; GNU Affero General Public License for more details.
;
; You should have received a copy of the GNU Affero General Public License
; along with this program.  If not, see <http://www.gnu.org/licenses/>.

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

(assert (= (funcall (lambda (&key k (j "foo") &all-keys keys) (get keys "k")) :k 1) 1))
(assert (= (funcall (lambda (&key (k 3) (j k) &all-keys keys) (get keys "j"))) nil))

;;; AND

(assert (= #t (and)))
(assert (= #t (and #t)))
(assert (= #f (and #f)))
(assert (= #f (and #t #f)))
(assert (= #f (and #f #t)))
(assert (= #t (and #t #t)))
(assert (= 1 (and 1)))
(assert (= 3 (and 1 2 3)))

;;; OR

(assert (= #f (or)))
(assert (= #f (or #f)))
(assert (= #t (or #t)))
(assert (= #t (or #f #t)))
(assert (= #t (or #t #f)))
(assert (= #t (or #t #t #f)))
(assert (= 1 (or 1 #t #f)))
(assert (= 3 (or #f #f 3)))

;;; COND

(assert (= #f (cond)))
(assert (= 1 (cond (#t 1))))
(assert (= 2 (cond (#f 1) (#t 2))))
(assert (= #f (cond (#f 1) (#f 2))))

;;; Dynamic Variables

(defdynamic test:dyn1)
(setf (dynamic test:dyn1) 12)
(assert (= 12 (dynamic test:dyn1)))
(dynamic-bind ((test:dyn1 13))
  (assert (= 13 (dynamic test:dyn1))))
(assert (= 12 (dynamic test:dyn1)))

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

;;; Options

(assert (= 2 (if-option (x none) 1 2)))
(assert (= 1 (if-option (x (option 1)) x 2)))

;;; SHA1

(assert (= "0beec7b5ea3f0fdbc95d0dd47f3c5bc275da8a33"
           (sha1 "foo")))

(provide "test-boot")
