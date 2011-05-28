;;; A stupid Scheme compatibility mode for SRFI 72 examples and tests.
;;; Using it requires loading the file lisp-1.js before runtime.js.

;; (define-syntax ( name . args) . body)
;;  0             1 0      1       2
(%%defmacro define-syntax
  (%%lambda (ds)
    #`(%%defmacro ,(%compound-elt (%compound-elt ds 1) 0)
        (%%lambda (form)
          (%compound-apply
           (%%lambda ,(%compound-slice (%compound-elt ds 1) 1)
             (%%progn
               ,@(%compound-slice ds 2)))
           (%compound-slice form 1))))))

(define-syntax (define name value)
  #`(%%defparameter ,name ,value))

(define-syntax (begin . forms)
  #`(%%progn ,@forms))

(define-syntax (lambda sig . body)
  #`(%%lambda ,sig (begin ,@body)))

(define-syntax (funcall function . arguments)
  #`(%%funcall ,function ,@arguments))

(define-syntax (if test consequent &optional (alternative #'nil))
  #`(%%if ,test ,consequent ,alternative))

(define-syntax (set! name value)
  #`(%%setq ,name ,value))

(define-syntax (let bindings . body)
  #`(funcall (lambda ,(%compound-map (lambda (b)
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

(define-syntax (begin-for-syntax . forms)
  #`(%%eval-when-compile (begin ,@forms)))

(define alert
  (lambda (message)
    #{ alert(~message) #}))

(begin-for-syntax
 (define car
   (lambda (form)
     (%compound-elt form 0)))
 
 (define cdr
   (lambda (form)
     (%compound-slice form 1)))
 
 (define null?
   (lambda (list)
     (%list-empty? list)))
 
 (define literal-identifier=?
   (lambda (a b)
     (%literal-identifier-equal? a b))))

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

