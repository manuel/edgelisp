;;;; Bootstrap defmacro

(%%defsyntax defmacro
  (%%lambda (defmacro-form)
    #`(%%defsyntax ,(compound-elt defmacro-form 1)
        (%%lambda (%%form)
          (compound-apply
            (%%lambda ,(compound-elt defmacro-form 2)
              (%%progn
                ,@(compound-slice defmacro-form 3)))
            (compound-slice %%form 1))))))

;;;; Wrap special forms

(defmacro defined? (identifier)
  #`(%%defined? ,identifier))

(defmacro defparameter (name value)
  #`(%%defparameter ,name ,value))

(defmacro defsyntax (name transformer)
  #`(%%defsyntax ,name ,transformer))

(defmacro eval-when-compile (&rest forms)
  #`(%%eval-when-compile (%%progn ,@forms)))

(defmacro fdefined? (name)
  #`(defined? (%%identifier ,name function)))

(defmacro funcall (function &rest arguments)
  #`(%%funcall ,function ,@arguments))

(defmacro function (name)
  #`(%%identifier ,name function))

(defmacro identifier (name namespace)
  #`(%%identifier ,name ,namespace))

(defmacro if (test consequent &optional (alternative #'nil))
  #`(%%if ,test ,consequent ,alternative))

(defmacro lambda (sig &rest body)
  #`(%%lambda ,sig (%%progn ,@body)))

(defmacro native (&rest body)
  #`(%%native ,@body))

(defmacro native-snippet (string)
  #`(%%native-snippet ,string))

(defmacro progn (&rest body)
  #`(%%progn ,@body))

(defmacro setq (name value)
  #`(%%setq ,name ,value))

;;;; Lexical variables

(defmacro defvar (name &optional (value #'nil))
  #`(defparameter ,name (if (defined? ,name) ,name ,value)))

(defmacro defun (name sig &rest body)
  #`(defparameter (function ,name) (lambda ,sig ,@body)))

(defmacro let (bindings &rest body)
  #`(funcall (lambda ,(compound-map (lambda (b) 
                                     (compound-elt b 0))
                                   bindings)
              ,@body)
            ,@(compound-map (lambda (b) 
                              (compound-elt b 1)) 
                            bindings)))

(defmacro let* (bindings &rest forms) ;; Scheme's letrec*
  #`(let ,(compound-map (lambda (b)
                          #`(,(compound-elt b 0) #{ undefined #}))
                        bindings)
      ,@(compound-map (lambda (b)
                        #`(setq ,(compound-elt b 0) ,(compound-elt b 1)))
                      bindings)
      ,@forms))

;;;; Control flow

(defmacro when (test &rest consequent)
  #`(if ,test (progn ,@consequent) nil))

(defmacro unless (test &rest alternative)
  #`(if ,test nil (progn ,@alternative)))

(defmacro loop (&rest body)
  #`(call-forever (lambda () ,@body)))

(defmacro catch (tag &rest body)
  #`(call-with-catch-tag ,tag (lambda () ,@body)))

(defmacro block (bname &rest forms)
  #`(let ((,bname (list)))
      (catch ,bname ,@forms)))

(defmacro return-from (bname &optional (exp #'nil))
  #`(throw ,bname ,exp))

(defmacro unwind-protect (protected &rest cleanups)
  #`(call-unwind-protected (lambda () ,protected)
                           (lambda () ,@cleanups)))

(defmacro while (test &rest body)
  #`(block exit
      (loop
         (unless ,test (return-from exit))
         ,@body)))

(defmacro or (&rest forms)
  (if (list-empty? forms) ; heck, need destructuring-bind
      #'#f ; heck, need to rethink #' syntax
      #`(let ((%%or-res ,(list-elt forms 0))) ; heck, need hygiene
          (if %%or-res
              %%or-res
              (or ,@(list-slice forms 1))))))

(defmacro and (&rest forms)
  ;; same hecks as above apply
  ;; fixme: should return value of last subform
  (if (list-empty? forms)
      #'#t
      #`(if (not ,(list-elt forms 0))
            #f
            (and ,@(list-slice forms 1)))))

(defun not ((x object))
  (if x #f #t))

;;;; (Slightly) Generalized references

(eval-when-compile
  (defun setter-name (getter-name)
    (string-concat getter-name "-setter")))

(defmacro setf (place value)
  (if (symbol? place)
      #`(setq ,place ,value)
      #`(,(string-to-symbol (setter-name (symbol-name (compound-elt place 0))))
        ,@(compound-slice place 1)
        ,value)))

(defmacro incf (place &optional (delta #'1))
  #`(setf ,place (+ ,place ,delta)))

(defmacro decf (place &optional (delta #'1))
  #`(setf ,place (- ,place ,delta)))

;;;; Stuff

(defun nil? ((x object))
  (if (eq nil x) #t #f))

(defmacro assert (test &optional (msg #'"assertion failed"))
  #`(let ((result ,test))
     (if result
         result
         (progn
           (print ,msg)
           (print #',test)))))

(defmacro assert-eq (a b)
  #`(assert (eq ,a ,b)))

(defmacro eval-and-compile (&rest forms)
  #`(progn
     (eval-when-compile ,@forms)
     ,@forms))

(defmacro native-body (&rest stuff)
  #`#{ (function(){ ~,@stuff })() #})

;;;; Dynamic variables

(defmacro dynamic (name)
  #`(identifier ,name dynamic))

(defmacro defdynamic (name &optional (value #'nil))
  #`(defvar (dynamic ,name) ,value))

(defmacro dynamic-bind (bindings &rest body)
  (if (compound-empty? bindings)
      #`(progn ,@body)
      #`(dynamic-bind-1 ,(compound-elt bindings 0)
          (dynamic-bind ,(compound-slice bindings 1)
             ,@body))))

(defmacro dynamic-bind-1 (binding &rest body)
  (let ((name (compound-elt binding 0))
        (value (compound-elt binding 1)))
    #`(let ((old-value (dynamic ,name)))
        (setq (dynamic ,name) ,value)
        (unwind-protect (progn ,@body)
          (setq (dynamic ,name) old-value)))))

;;;; Object system

(defmacro class (name)
  #`(identifier ,name class))

(defmacro generic (name)
  #`(identifier ,name generic))

(defmacro make (class-name)
  #`(make-instance (class ,class-name)))

(defmacro defclass (class-name &optional (supers #'()) (slots #'()))
  (let ((superclass (if (compound-empty? supers) #f (compound-elt supers 0))))
    #`(progn
        (defvar (class ,class-name) (make-class))
        (set-class-name (class ,class-name)
                        ,(string-to-form (symbol-name class-name)))
        ,(if superclass 
             #`(set-superclass (class ,class-name) (class ,superclass))
             #`(set-superclass (class ,class-name) (class object)))
        ,@(compound-map (lambda (slot)
                          #`(defslot ,slot ,class-name))
                        slots)
        (class ,class-name))))

(defmacro defgeneric (name &rest args)
  #`(progn
      (defvar (generic ,name) (make-generic ,(string-to-form (symbol-name name))))
      (defun ,name (&fast fast-arguments)
        (let ((method (find-method (generic ,name) fast-arguments)))
          (fast-apply method fast-arguments)))))

(defmacro defmethod (name params &rest body)
  #`(progn
      (defgeneric ,name)
      (put-method (generic ,name)
                  (list ,@(params-specializers params))
                  (lambda ,params ,@body))
      #',name))

(defmacro defslot (slot class)
  (let ((setter-name (setter-name (symbol-name slot)))
        (slot-name-form (string-to-form (symbol-name slot))))
    #`(progn
        (defmethod ,slot ((obj ,class))
          (slot-value obj ,slot-name-form))
        (defmethod ,(string-to-symbol setter-name) ((obj ,class) value)
          (set-slot-value obj ,slot-name-form value)))))

;;;; Fixup class hierarchy

(defmacro define-builtin-class (name superclass)
  #`(set-superclass (class ,name) (class ,superclass)))

(define-builtin-class big-integer integer)
(define-builtin-class boolean literal)
(define-builtin-class class object)
(define-builtin-class compound-form form)
(define-builtin-class error object)
(define-builtin-class form object)
(define-builtin-class function object)
(define-builtin-class generic object)
(define-builtin-class integer rational)
(define-builtin-class list object)
(define-builtin-class literal object)
(define-builtin-class nil literal)
(define-builtin-class number literal)
(define-builtin-class number-form form)
(define-builtin-class rational real)
(define-builtin-class real number)
(define-builtin-class simple-error error)
(define-builtin-class small-integer integer)
(define-builtin-class string literal)
(define-builtin-class string-dict object)
(define-builtin-class string-form form)
(define-builtin-class symbol-form form)

;;;; Equality

(defgeneric = (a b))

(defmethod = ((a object) (b object))
  (eq a b))

;;;; Printing

(defgeneric show (object))

(defmethod show ((a object))
  (string-concat "#[" (class-name (type-of a)) " " (show-object a) "]"))

(defmethod show ((a literal))
  (show-object a))

(defgeneric show-object (object))

(defmethod show-object ((a object))
  #{ JSON.stringify(~a) #})

(defmethod show-object ((a nil))
  "nil")

(defmethod show-object ((a boolean))
  (if a "#t" "#f"))

(defmethod show-object ((a number))
  #{ (~a).toString() #})

(defmethod show-object ((a form))
  #{ (~a).toString() #})

(defmethod show-object ((a function))
  #{ (~a).toString() #})

(defmethod show-object ((a class))
  (class-name a))

;;;; Numbers

(defmacro define-jsnums-binop (name jsnums-name)
  #`(progn
      (defgeneric ,name (a b))
      (defmethod ,name ((a number) (b number))
        #{ jsnums.~(native-snippet ,jsnums-name)(~a, ~b) #})))

(define-jsnums-binop = "equals")
(define-jsnums-binop > "greaterThan")
(define-jsnums-binop < "lessThan")
(define-jsnums-binop / "divide")
(define-jsnums-binop * "multiply")
(define-jsnums-binop + "add")
(define-jsnums-binop - "subtract")

(defgeneric to-number (object))

(defmethod to-number ((s string))
  (string-to-number s))

;;;; Condition system

(defclass condition)
(defclass serious-condition (condition))
(defclass error (serious-condition))
(defclass warning (condition))

;;; Specific errors
(defclass unbound-variable (error)
  (.name
   .namespace))
(defun make-unbound-variable ((name string) (namespace string))
  (let ((e (make unbound-variable)))
    (setf (.name e) name)
    (setf (.namespace e) namespace)
    e))
(defmethod show ((e unbound-variable))
  (string-concat "The " (.namespace e) " " (.name e) " is unbound."))
(defclass simple-error (error))
(defmethod show ((e simple-error))
  (string-concat (simple-error-message e) ": " (simple-error-arg e)))
(defclass control-error (error))

(defclass restart (condition)
  (.associated-condition))

;;; Specific restarts
(defclass abort (restart))
(defclass continue (restart))
(defclass use-value (restart)
  (.value))
(defclass store-value (restart)
  (.value))

(defgeneric default-handler (condition))
(defmethod default-handler ((c condition))
  nil)
(defmethod default-handler ((c warning))
  (print c))
(defmethod default-handler ((c serious-condition))
  (invoke-debugger c))
(defmethod default-handler ((c restart))
  (error (make control-error)))

(defclass handler ()
  (.handler-class
   .handler-function
   .associated-condition))

(defmethod show ((h handler))
  (show-object (.handler-class h)))

(defun make-handler ((handler-class class) (handler-function function)
                     &optional (associated-condition nil))
  (let ((h (make handler)))
    (setf (.handler-class h) handler-class)
    (setf (.handler-function h) handler-function)
    (setf (.associated-condition h) associated-condition)
    h))

(defclass handler-frame ()
  (.handlers
   .parent-frame))

(defun make-handler-frame ((handlers list) &optional (parent-frame nil))
  (let ((f (make handler-frame)))
    (setf (.handlers f) handlers)
    (setf (.parent-frame f) parent-frame)
    f))

(defdynamic handler-frame)

(defmacro handler-bind (specs &rest body)
  "spec ::= (class-name function-form)"
  #`(dynamic-bind ((handler-frame
                    (make-handler-frame
                     (list ,@(compound-map
                              (lambda (spec)
                                #`(make-handler
                                   (class ,(compound-elt spec 0))
                                   ,(compound-elt spec 1)))
                              specs)))))
      ,@body))

(defun signal ((c condition))
  (signal-condition c (dynamic handler-frame)))

(defdynamic restart-frame)

(defmacro restart-bind (specs &rest body)
  "spec ::= (class-name function-form associated-condition)"
  #`(dynamic-bind ((restart-frame
                    (make-handler-frame
                     (list ,@(compound-map
                              (lambda (spec)
                                #`(make-handler
                                   (class ,(compound-elt spec 0))
                                   ,(compound-elt spec 1)
                                   ,(compound-elt spec 2)))
                              specs)))))
      ,@body))

(defun invoke-restart ((r restart))
  (signal-condition r (dynamic restart-frame)))

(defun signal-condition ((c condition) f)
  (let* ((handler-and-frame (find-applicable-handler-and-frame c f)))
    (if (nil? handler-and-frame)
        (default-handler c)
        (let ((h (elt handler-and-frame 0))
              (f (elt handler-and-frame 1)))
          (call-condition-handler c h f)
          ;; signal unhandled: continue search for handlers
          (signal-condition c (.parent-frame f))))))

(defun find-applicable-handler-and-frame ((c condition) f)
  (when f
    (block found
      (each (lambda (h)
              (when (condition-applicable? c h)
                (return-from found (list h f))))
            (.handlers f))
      (find-applicable-handler-and-frame c (.parent-frame f)))))

(defgeneric condition-applicable? (condition handler))

(defmethod condition-applicable? ((c condition) (h handler))
  (subtype? (type-of c) (.handler-class h)))

(defmethod condition-applicable? ((r restart) (h handler))
  (and (subtype? (type-of r) (.handler-class h))
       (or (nil? (.associated-condition r))
           (nil? (.associated-condition h))
           (eq (.associated-condition r) (.associated-condition h)))))

(defgeneric call-condition-handler (condition handler handler-frame))

(defmethod call-condition-handler ((c condition) (h handler) (f handler-frame))
  (dynamic-bind ((handler-frame (.parent-frame f))) ; condition firewall
    (funcall (.handler-function h) c)))

(defmethod call-condition-handler ((r restart) (h handler) (f handler-frame))
  (funcall (.handler-function h) r))

(defparameter \error \signal)
(defparameter \warn \signal)

(defun compute-restarts ((c condition))
  (lisp:compute-restarts c))

(defun lisp:compute-restarts ((c condition)
                              &optional (l (list)) (f (dynamic restart-frame)))
  (when f
    (each (lambda (h)
            (when (or (nil? (.associated-condition h))
                      (eq c (.associated-condition h)))
              (add l h)))
          (.handlers f))
    (lisp:compute-restarts c l (.parent-frame f)))
  l)

(defun undefined-identifier ((name string) (namespace string))
  "Called by the runtime."
  (block return
    (let ((c (make-unbound-variable name namespace)))
      (restart-bind ((use-value (lambda (r)
                                  (return-from return (prompt "Use value")))
                       c))
        (error c)))))

;;;; Debugger

(defun invoke-debugger ((c condition))
  (print c)
  (print "Restarts:")
  (let ((restarts (compute-restarts c)) (i 0))
    (each (lambda (r)
            (print (string-concat i ": " (show r)))
            (incf i))
          restarts)
    (let ((s (prompt "Enter a restart number, or cancel to abort:")))
      (if (nil? s)
          (lisp:hard-abort c)
          (let ((n (to-number s)))
            (invoke-restart (make-instance (.handler-class (elt restarts n)))))))))

(defun lisp:hard-abort ((c condition))
  (native-body #{ throw ~c #}))

(defun prompt (&optional (s "Lisp prompt"))
  "Returns user-entered string or nil."
  #{ prompt(~s) #})

(defparameter \original-no-applicable-method \no-applicable-method)
(defun no-applicable-method (generic arguments)
  (original-no-applicable-method generic arguments))

(defparameter \original-no-most-specific-method \no-most-specific-method)
(defun no-most-specific-method (generic arguments)
  (original-no-most-specific-method generic arguments))

;;;; Streams


;;;; Collections

(defgeneric elt (collection index))
(defgeneric empty? (collection))
(defgeneric slice (collection from-index))

(defmethod elt ((c compound-form) (i small-integer))
  (compound-elt c i))

(defmethod empty? ((c compound-form))
  (compound-empty? c))

(defmethod slice ((c compound-form) (from small-integer))
  (compound-slice c from))

(defclass dict)

(defgeneric get (dict key &optional default))
(defgeneric put (dict key value))
(defgeneric has-key (dict key))

(defclass string-dict (dict))

(defmethod get ((dict string-dict) (key string) &optional default)
  (if (has-key dict key)
      (string-dict-get dict key)
      default))

(defmethod put ((dict string-dict) (key string) value)
  (string-dict-put dict key value))

(defmethod has-key ((dict string-dict) (key string))
  (string-dict-has-key dict key))

(defclass list)

(defmethod iter ((list list))
  (list-iter list))

(defmethod len ((list list))
  (list-len list))

(defmethod elt ((list list) (i number))
  (list-elt list i))

(defmethod add ((list list) elt)
  (list-add list elt))

(defclass iter)

(defmethod iter ((iter iter))
  iter)

(defclass list-iter (iter)
  (.list
   .i))

(defun list-iter ((list list))
  (let ((iter (make list-iter)))
    (setf (.list iter) list)
    (setf (.i iter) 0)
    iter))

(defmethod has-next ((iter list-iter))
  (< (.i iter) (len (.list iter))))

(defmethod next ((iter list-iter))
  (incf (.i iter)))

(defmethod now ((iter list-iter))
  (elt (.list iter) (.i iter)))

(defmethod elt ((form compound-form) i)
  (compound-elt form i))

(defmethod iter ((form compound-form))
  (iter (compound-elts form)))

(defun every ((pred function) coll)
  "Returns #t iff every element of a collection satisfies the predicate."
  (block exit
    (let ((iter (iter coll)))
      (while (has-next iter)
        (unless (funcall pred (now iter))
          (return-from exit #f))
        (next iter)))
    #t))

(defun each ((fun function) &rest colls)
  "Applies a function to the elements of one or more collections for
effect.  The function is called with N positional arguments, each
taken from the collections from left to right.  The shortest
collection determines how many times the function is called."
  (if (= 1 (len colls))
      (let ((iter (iter (elt colls 0))))
        (while (has-next iter)
          (funcall fun (now iter))
          (next iter)))
      (let ((iters (map \iter colls)))
        (while (every \has-next iters)
          (apply fun (map \now iters))
          (each \next iters)))))

(defun map ((fun function) &rest colls &key (into (list)))
  "Applies a function to the elements of one or more collections and
returns a collection with the results of each application.  The
function is called with N positional arguments, each taken from the
collections from left to right.  The shortest collection determines
how many times the function is called.  The #`into' keyword argument
can be used to supply a different collection to hold the results."
  (if (= 1 (len colls))
      (let ((iter (iter (elt colls 0))))
        (while (has-next iter)
          (add into (funcall fun (now iter)))
          (next iter)))
      (let ((iters (map \iter colls)))
        (while (every \has-next iters)
          (add into (apply fun (map \now iters)))
          (each \next iters))))
  into)

(defun string-join ((separator string) &rest strings)
  (let ((iter (iter strings)))
    (if (has-next iter)
        (let ((result (now iter)))
          (next iter)
          (while (has-next iter)
            (setq result (string-concat result separator (now iter)))
            (next iter))
          result)
        "")))

(defclass number-iter (iter)
  (.i
   .max))

(defun make-number-iter (max)
  (let ((iter (make number-iter)))
    (setf (.i iter) 0)
    (setf (.max iter) max)
    iter))

(defmethod iter ((max number))
  (make-number-iter max))

(defmethod has-next ((iter number-iter))
  (< (.i iter) (.max iter)))

(defmethod now ((iter number-iter))
  (.i iter))

(defmethod next ((iter number-iter))
  (incf (.i iter)))

(defmacro dotimes (var-and-ct &rest body)
  (let ((var (elt var-and-ct 0))
        (ct (elt var-and-ct 1)))
    #`(each (lambda (,var) ,@body) ,ct)))

(print "Lisp bootstrapped")
