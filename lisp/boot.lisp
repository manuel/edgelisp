;;;; Bootstrap defmacro

(%%defparameter %%hygiene-context nil)

(%%defsyntax defmacro
  (%%lambda (defmacro-form)
    #`(%%defsyntax ,(%compound-elt defmacro-form 1)
        (%%lambda (%%form)
          (%compound-apply
           (%%lambda ,(%compound-elt defmacro-form 2)
             (%%progn
               ,@(%compound-slice defmacro-form 3)))
           (%compound-slice %%form 1))))))

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

(defmacro let* (bindings &rest forms) ;; Scheme's letrec*
  #`(let ,(%compound-map (lambda (b)
                           #`(,(%compound-elt b 0) #{ undefined #}))
                         bindings)
      ,@(%compound-map (lambda (b)
                         #`(setq ,(%compound-elt b 0) ,(%compound-elt b 1)))
                       bindings)
      ,@forms))

;;;; Utilities

(defmacro eval-and-compile (&rest forms)
  #`(progn
      (eval-when-compile ,@forms)
      ,@forms))

(defmacro assert (test &optional (msg #'"assertion failed"))
  #`(let ((result ,test))
     (if result
         result
         (progn
           (print ,msg)
           (print #',test)))))

(defmacro native-body (&rest stuff)
  #`#{ (function(){ ~,@stuff })() #})

(defun nil? ((a object))
  (if (eq nil a) #t #f))

;;;; Wrap built-in functions

(eval-and-compile

(defun append-compounds (&rest arguments)
  (apply \%append-compounds arguments))

(defun apply ((f function) (args list) &optional (keys nil))
  (%apply f args keys))

(defun call-unwind-protected ((protected function) (cleanup function))
  (%call-unwind-protected protected cleanup))

(defun call-with-catch-tag ((tag object) (body function))
  (%call-with-catch-tag tag body))

(defun call-forever ((f function))
  (%call-forever f))

(defun class-name ((c class))
  (%class-name c))

(defun compound-apply ((f function) (cf compound-form))
  (%compound-apply f cf))

(defun compound-add ((cf compound-form) (f form))
  (%compound-add cf f))

(defun compound-elt ((cf compound-form) (i small-integer))
  (%compound-elt cf i))

(defun compound-elts ((cf compound-form))
  (%compound-elts cf))

(defun compound-empty? ((cf compound-form))
  (%compound-empty? cf))

(defun compound-len ((cf compound-form))
  (%compound-len cf))

(defun compound-map ((f function) (cf compound-form))
  (%compound-map f cf))

(defun compound-slice ((cf compound-form) (start small-integer)
                       &optional (end (compound-len cf)))
  (%compound-slice cf start end))

(defun compound? ((a object))
  (%compound? a))

(defun eq ((a object) (b object))
  (%eq a b))

(defun eval ((form form))
  (%eval form))

(defparameter \fast-apply \%fast-apply) ; fishy?

(defparameter \find-method \%find-method) ; fishy

(defun has-slot ((a object) (slot-name string))
  (%has-slot a slot-name))

(defun identifier-name ((s identifier-form))
  (%identifier-name s))

(defun identifier? ((a object))
  (%identifier? a))

(defun list (&rest arguments)
  (apply \%list arguments))

(defun list-add ((l list) (a object))
  (%list-add l a))

(defun list-elt ((l list) (i small-integer))
  (%list-elt l i))

(defun list-empty? ((l list))
  (%list-empty? l))

(defun list-len ((l list))
  (%list-len l))

(defun list-slice ((l list) (start small-integer)
                   &optional (end (list-len l)))
  (%list-slice l start end))

(defun macroexpand-1 ((form form))
  (%macroexpand-1 form))

(defun make-class ()
  (%make-class))

(defun make-compound (&rest arguments)
  (apply \%make-compound arguments))

(defun make-generic ((name string))
  (%make-generic name))

(defun make-uuid ()
  (%make-uuid))

(defun note ((a object))
  (%note a))

(defun params-specializers ((cf compound-form))
  (%params-specializers cf))

(defun print ((a object))
  (%print a))

(defun put-method ((g generic) (specializers list) (m function))
  (%put-method g specializers m))

(defun read-from-string ((s string))
  (%read-from-string s))

(defun set-class-name ((c class) (s string))
  (%set-class-name c s))

(defun set-slot-value ((a object) (slot-name string) (value object))
  "Caution: bypasses slot type check"
  (%set-slot-value a slot-name value))

(defun set-superclass ((class class) (superclass class))
  (%set-superclass class superclass))

(defun simple-error-message ((e simple-error))
  (%simple-error-message e))

(defun simple-error-arg ((e simple-error))
  (%simple-error-arg e))

(defun slot-value ((a object) (slot-name string))
  (%slot-value a slot-name))

(defun string-concat (&rest arguments)
  (apply \%string-concat arguments))

(defun string-dict-get ((d string-dict) (key string))
  (%string-dict-get d key))

(defun string-dict-has-key ((d string-dict) (key string))
  (%string-dict-has-key d key))

(defun string-dict-map ((f function) (d string-dict))
  (%string-dict-map f d))

(defun string-dict-put ((d string-dict) (key string) (value object))
  (%string-dict-put d key value))

(defun string-len ((s string))
  (%string-len s))

(defun string-to-form ((s string))
  (%string-to-form s))

(defun string-to-identifier ((s string))
  (%string-to-identifier s))

(defun string-to-number ((s string))
  (%string-to-number s))

(defun subtype? ((class class) (possible-superclass class))
  (%subtype? class possible-superclass))

(defun superclass ((c class))
  (%superclass c))

(defun type-of ((a object))
  (%type-of a))

) ; eval-and-compile

;;;; Control flow

(defmacro when (test &rest consequent)
  #`(if ,test (progn ,@consequent) nil))

(defmacro unless (test &rest alternative)
  #`(if ,test nil (progn ,@alternative)))

(defmacro loop (&rest body)
  #`(call-forever (lambda () ,@body)))

(defmacro catch (tag &rest body)
  #`(call-with-catch-tag ,tag (lambda () ,@body)))

(defun throw ((tag object) &optional (value nil))
  (%throw tag value))

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
  (if (identifier? place)
      #`(setq ,place ,value)
      #`(,(string-to-identifier (setter-name (identifier-name (compound-elt place 0))))
        ,@(compound-slice place 1)
        ,value)))

(defmacro incf (place &optional (delta #'1))
  #`(setf ,place (+ ,place ,delta)))

(defmacro decf (place &optional (delta #'1))
  #`(setf ,place (- ,place ,delta)))

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

(defun make-instance (class &all-keys init-args)
  (let ((obj (%make-instance class)))
    (string-dict-map (lambda (slot-name)
                       (set-slot-value obj slot-name (string-dict-get init-args slot-name)))
                     init-args)
    obj))

(defmacro make (class-name &rest init-args)
  #`(make-instance (class ,class-name) ,@init-args))

(defmacro defclass (class-name &optional (supers #'()) (slot-descriptors #'())
                    &rest ignored)
  (let ((superclass (if (compound-empty? supers) #f (compound-elt supers 0))))
    #`(progn
        (defvar (class ,class-name) (make-class))
        (set-class-name (class ,class-name)
                        ,(string-to-form (identifier-name class-name)))
        ,(if superclass 
             #`(set-superclass (class ,class-name) (class ,superclass))
             #`(set-superclass (class ,class-name) (class object)))
        ,@(compound-map (lambda (slot-descriptor)
                          #`(defslot ,slot-descriptor ,class-name))
                        slot-descriptors)
        (class ,class-name))))

(defmacro defgeneric (name &rest args)
  #`(progn
      (defvar (generic ,name) (make-generic ,(string-to-form (identifier-name name))))
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

(defmacro defslot (slot-descriptor class)
  (let* ((slot-name
          (if (compound? slot-descriptor)
              (identifier-name (compound-elt slot-descriptor 0))
              (identifier-name slot-descriptor)))
         (accessor-name (string-concat "." slot-name))
         (setter-name (setter-name accessor-name))
         (slot-name-form (string-to-form slot-name)))
    #`(progn
        (defmethod ,(string-to-identifier accessor-name) ((obj ,class))
          (slot-value obj ,slot-name-form))
        (defmethod ,(string-to-identifier setter-name) ((obj ,class) value)
          (set-slot-value obj ,slot-name-form value)))))

;;;; Equality

(defgeneric = (a b))

(defmethod = ((a object) (b object))
  (eq a b))

;;;; Printing

(defgeneric show (object))

(defmethod show ((a object))
  (let ((details (show-object a)))
    (string-concat "#["
                   (class-name (type-of a)) 
                   (if (> (string-len details) 0) " [" "")
                   details
                   (if (> (string-len details) 0) "]" "")
                   "]")))

(defmethod show ((a literal))
  (show-object a))

(defmethod show ((a form))
  (string-concat "#'" #{ (~a).toString() #}))


(defgeneric show-object (object))

(defmethod show-object ((a object))
  (show-raw a))

(defmethod show-object ((a nil))
  "nil")

(defmethod show-object ((a boolean))
  (if a "#t" "#f"))

(defmethod show-object ((a number))
  #{ (~a).toString() #})

(defmethod show-object ((a function))
  #{ (~a).toString() #})

(defmethod show-object ((a class))
  (class-name a))


(defun show-raw ((a object))
  #{ JSON.stringify(~a) #})

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

;;;; Condition system

(defclass condition)
(defclass serious-condition (condition))
(defclass error (serious-condition))
(defclass warning (condition))

;;; Specific errors
(defclass unbound-variable (error)
  (name
   namespace))
(defun make-unbound-variable ((name string) (namespace string))
  (make unbound-variable :name name :namespace namespace))
(defmethod show ((e unbound-variable))
  (string-concat "The " (.namespace e) " " (.name e) " is unbound."))
(defclass simple-error (error))
(defmethod show-object ((e simple-error))
  (string-concat (simple-error-message e) ": " (simple-error-arg e)))
(defclass control-error (error))
(defclass restart-control-error (control-error)
  (restart))
(defmethod show-object ((e restart-control-error))
  (string-concat "No restart handler for " (show (.restart e))))

(defclass restart (condition)
  (associated-condition))
(defmethod show-object ((r restart))
  "")

;;; Specific restarts
(defclass abort (restart))
(defclass continue (restart))
(defclass use-value (restart)
  (value))
(defclass store-value (restart)
  (value))

(defgeneric default-handler (condition))
(defmethod default-handler ((c condition))
  nil)
(defmethod default-handler ((c warning))
  (print c))
(defmethod default-handler ((c serious-condition))
  (invoke-debugger c))
(defmethod default-handler ((r restart))
  (error (make restart-control-error :restart r)))

(defclass handler ()
  (handler-class
   handler-function
   associated-condition))

(defmethod show-object ((h handler))
  (show-object (.handler-class h)))

(defun make-handler ((c class) (f function)
                     &optional (associated-condition nil))
  (make handler
        :handler-class c
        :handler-function f
        :associated-condition associated-condition))

(defclass handler-frame ()
  (handlers
   parent-frame))

(defun make-handler-frame ((handlers list) &optional (parent-frame nil))
  (make handler-frame :handlers handlers :parent-frame parent-frame))

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
                              specs))
                     (dynamic handler-frame))))
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
                              specs))
                     (dynamic restart-frame))))
      ,@body))

(defun invoke-restart ((r restart))
  (signal-condition r (dynamic restart-frame)))

(defgeneric invoke-restart-interactively (restart))

(defmethod invoke-restart-interactively ((r restart))
  (invoke-restart r))

(defmethod invoke-restart-interactively ((r use-value))
  (loop (block in-loop
          (let* ((res (prompt "Enter value to use, or cancel to abort:")))
            (if (nil? res)
                (abort)
                (handler-bind ((error (lambda (e)
                                        (return-from in-loop))))
                  (setf (.value r) (eval (read-from-string res)))
                  (invoke-restart r)))))))

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

(defun abort ()
  (invoke-restart (make abort)))

(defun compute-restarts ((c condition))
  (do-compute-restarts c))

(defun do-compute-restarts ((c condition)
                            &optional (l (list)) (f (dynamic restart-frame)))
  (when f
    (each (lambda (h)
            (when (or (nil? (.associated-condition h))
                      (eq c (.associated-condition h)))
              (add l h)))
          (.handlers f))
    (do-compute-restarts c l (.parent-frame f)))
  l)

(defun undefined-identifier ((name string) (namespace string))
  "Called by the runtime."
  (block return
    (let ((c (make-unbound-variable name namespace)))
      (restart-bind ((use-value (lambda (r)
                                  (return-from return (.value r)))
                       c))
        (error c)))))

;;;; Debugger

(defun invoke-debugger ((c condition))
  (note (string-concat "Condition: " (show c)))
  (let ((restarts (compute-restarts c)))
    (if (> (list-len restarts) 0)
        (progn
          (note "Restarts:")
          (let ((i 1))
            (each (lambda (r)
                    (note (string-concat i ": " (show r)))
                    (incf i))
                  restarts)
            (let ((s (prompt "Enter a restart number, or cancel to abort:")))
              (if (nil? s)
                  (abort)
                  (let ((n (string-to-number s)))
                    (invoke-restart-interactively
                     (make-instance (.handler-class (elt restarts (- n 1))))))))))
        (hard-abort c))))

(defun hard-abort ((c condition))
  (note "Aborting hard")
  (native-body #{ throw ~c #}))

(defun alert (&optional (s "Lisp alert"))
  #{ alert(~s) #})

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

(defmethod show ((a list))
  (string-concat "(" (string-join-list " " (map \show a)) ")"))

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
  (list
   i))

(defun list-iter ((list list))
  (make list-iter :list list :i 0))

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
  (string-join-list separator strings))

(defun string-join-list ((separator string) (strings list))
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
  (i
   max))

(defun make-number-iter (max)
  (make number-iter :i 0 :max max))

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

(defun provide ((what string))
  "This is really a joke atm as there is no module system."
  (note (string-concat "Provide " what)))

(provide "boot")
