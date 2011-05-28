;;;; Simple Scheme compatibility mode for EdgeLisp

;;; Of course, this is no real Scheme, as EdgeLisp has neither
;;; multi-shot continuations nor tail call elimination, but for some
;;; examples it works.
;;;
;;; Using it requires loading the file lisp-1.js before runtime.js.

;;; (define-syntax ( name &rest args) &rest body)
;;;  0             1 0          1           2
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

(define-syntax (begin &rest forms)
  #`(%%progn ,@forms))

(define-syntax (lambda sig &rest body)
  #`(%%lambda ,sig (begin ,@body)))

(define-syntax (funcall function &rest arguments)
  #`(%%funcall ,function ,@arguments))

(define-syntax (if test consequent &optional (alternative #'nil))
  #`(%%if ,test ,consequent ,alternative))

(define-syntax (set! name value)
  #`(%%setq ,name ,value))

(define-syntax (let bindings &rest body)
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

(define alert
  (lambda (message)
    #{ alert(~message) #}))

(define-syntax (swap! a b)
  #`(let ((temp ,a))
      (set! ,a ,b)
      (set! ,b temp)))

