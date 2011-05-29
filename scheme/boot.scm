;;; A stupid Scheme compatibility mode for running SRFI 72 examples
;;; and tests.  Using it requires loading the file lisp-1.js before
;;; runtime.js.

;; (define-syntax ( name . args) . body)
;;  0             1 0      1       2
;; (define-syntax name expander)
;;  0             1    2
(%%defmacro define-syntax
  (%%lambda (ds)
    (%%if (%compound? (%compound-elt ds 1))
          #`(%%defmacro ,(%compound-elt (%compound-elt ds 1) 0)
                        (%%lambda (form)
                                  (%compound-apply
                                   (%%lambda ,(%compound-slice (%compound-elt ds 1) 1)
                                             (%%progn
                                              ,@(%compound-slice ds 2)))
                                   (%compound-slice form 1))))
          #`(%%defmacro ,(%compound-elt ds 1)
                        ,(%compound-elt ds 2)))))

(define-syntax (define name value)
  #`(%%defparameter ,name ,value))

(define-syntax (begin . forms)
  #`(%%progn ,@forms))

(define-syntax (lambda sig . body)
  #`(%%lambda ,sig (begin ,@body)))

(define-syntax (if test consequent &optional (alternative #'nil))
  #`(%%if ,test ,consequent ,alternative))

(define-syntax (set! name value)
  #`(%%setq ,name ,value))

(define-syntax (begin-for-syntax . forms)
  #`(%%eval-when-compile (begin ,@forms)))

(begin-for-syntax
 (define car
   (lambda (form)
     (%compound-elt form 0)))
 
 (define cdr
   (lambda (form)
     (%compound-slice form 1)))
 
 (define cadr
   (lambda (form)
     (car (cdr form))))

 (define null?
   (lambda (list)
     (if (%compound? list)
         (%compound-empty? list)
         (%list-empty? list))))

 (define list %list)

 (define first
   (lambda (list)
     (%list-elt list 0)))

 (define rest
   (lambda (list)
     (%list-slice list 1)))

 (define literal-identifier=?
   (lambda (a b)
     (%literal-identifier-equal? a b))))

(define-syntax (let bindings . body)
  #`((lambda ,(%compound-map (lambda (b)
                               (if (%compound? b)
                                   (%compound-elt b 0)
                                   b))
                             bindings)
       ,@body)
     ,@(%compound-map (lambda (b)
                        (if (%compound? b)
                            (%compound-elt b 1)
                            #'nil))
                      bindings)))

(define-syntax (let* vs . forms)
  (if (null? vs)
      (let () ,@forms)
      #`(let (,(car vs)) (let* ,(cdr vs) ,@forms))))

(define-syntax (swap! a b)
  #`(let ((temp ,a))
      (set! ,a ,b)
      (set! ,b temp)))

(define-syntax (my-cond c . cs)
  (if (literal-identifier=? (car c) #'else)
      #`(begin ,@(cdr c))
      (if (null? cs)    
          #`(if ,(car c) (begin ,@(cdr c)))
          #`(if ,(car c)
                (begin ,@(cdr c))
                (my-cond ,@cs)))))

(define-syntax (main)
  (let ((make-swap (lambda (x y)
                     #`(let ((t ,x))
                         (set! ,x ,y)
                         (set! ,y t)))))
    #`(let ((s 1) (t 2))
        ,(make-swap #'s #'t)
        (list s t))))

