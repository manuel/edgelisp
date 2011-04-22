;; Bootstrap defmacro

(%%defsyntax defmacro
  (%%lambda (defmacro-form)
    #`(%%defsyntax ,(compound-elt defmacro-form 1)
        (%%lambda (%%form)
          (compound-apply
            (%%lambda ,(compound-elt defmacro-form 2)
              (%%progn
                ,@(compound-slice defmacro-form 3)))
            (compound-slice %%form 1))))))

;; Wrap special forms

(defmacro defined? (name)
  #`(%%defined? ,name))

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

(defmacro quasiquote (form)
  #`(%%quasiquote ,form))

(defmacro quote (form)
  #`(%%quote ,form))

(defmacro setq (name namespace)
  #`(%%setq ,name ,namespace))

;; Common stuff

(defmacro defvar (name &optional (value #'nil))
  #`(defparameter ,name (if (defined? ,name) ,name ,value)))

(defmacro defun (name sig &rest body)
  #`(defparameter (function ,name) (lambda ,sig ,@body)))

(defmacro when (test &rest consequent)
  #`(if ,test (progn ,@consequent) nil))

(defmacro unless (test &rest alternative)
  #`(if ,test nil (progn ,@alternative)))

(defmacro let (bindings &rest body)
  #`(funcall (lambda ,(compound-map (lambda (b) 
                                     (compound-elt b 0))
                                   bindings)
              ,@body)
            ,@(compound-map (lambda (b) 
                              (compound-elt b 1)) 
                            bindings)))

(defmacro let* (vs &rest forms)
  (if (compound-empty? vs)
      #`(let () ,@forms)
      #`(let (,(compound-elt vs 0))
          (let* ,(compound-slice vs 1) ,@forms))))

(defmacro while (test &rest body)
  #`(call-while (lambda () ,test) (lambda () ,@body)))

(defmacro block (name &rest body)
  #`(call-with-escape-function (lambda (,name) ,@body)))

(defmacro return-from (name &optional (value #'nil))
  #`(funcall ,name ,value))

(defmacro unwind-protect (protected &rest cleanups)
  #`(call-unwind-protected (lambda () ,protected)
                           (lambda () ,@cleanups)))

(defmacro handle (handler-specs &rest body)
  #`(bind-handlers (list ,@(compound-map (lambda (handler-spec)
                                           #`(list ,(compound-elt handler-spec 0)
                                                   ,(compound-elt handler-spec 1)))
                                         handler-specs))
                   (lambda () ,@body)))

(defun not (x)
  (if x #f #t))

(eval-when-compile
  (defun setter-name (getter-name)
    (string-concat getter-name "-setter")))

(defmacro set (place value)
  (if (symbol? place)
      #`(setq ,place ,value)
      #`(,(string-to-symbol (setter-name (symbol-name (compound-elt place 0))))
        ,@(compound-slice place 1)
        ,value)))

(defmacro inc (place &optional (delta #'1))
  #`(set ,place (+ ,place ,delta)))

(defmacro dec (place &optional (delta #'1))
  #`(set ,place (- ,place ,delta)))

(defmacro assert (test &optional (msg #'"assertion failed"))
  #`(let ((result ,test))
     (if result
         result
         (progn
           (print ,msg)
           (print #',test)))))

(defmacro assert-eq (a b)
  #`(assert (eq ,a ,b)))

(defmacro assert-eql (a b)
  #`(assert (eql ,a ,b)))

(defmacro eval-and-compile (&rest forms)
  #`(progn
     (eval-when-compile ,@forms)
     ,@forms))

(defmacro native-body (&rest stuff)
  #`#{ (function(){ ~,@stuff })() #})

;; OO

(eval-when-compile
  (defun defclass-do-slot (class-name slot)
    (let* ((slot-name (symbol-name slot))
           (slot-name-form (string-to-form slot-name))
           (getter-name (string-concat "." slot-name))
           (setter-name (setter-name getter-name)))
      #`(progn
          (defmethod ,(string-to-symbol getter-name) ((obj ,class-name))
            (slot obj ,slot-name-form))
          (defmethod ,(string-to-symbol setter-name) ((obj ,class-name) value)
            (set-slot obj ,slot-name-form value))))))

(defmacro class (name)
  #`(identifier ,name class))

(defmacro make (class-name)
  #`(make-instance (class ,class-name)))

(defmacro defclass (name-and-super &optional (slots #'()))
  (let ((class-name nil)
        (superclass nil))
    (if (symbol? name-and-super)
        (progn (set class-name name-and-super)
               (set superclass nil))
        (progn (set class-name (compound-elt name-and-super 0))
               (set superclass (compound-elt name-and-super 1))))
    #`(progn
        (defvar (class ,class-name) (make-class))
        ,(if superclass 
             #`(set-superclass (class ,class-name) (class ,superclass))
             #`(set-superclass (class ,class-name) (class object)))
        ,@(compound-map (lambda (slot)
                          (defclass-do-slot class-name slot))
                        slots)
        (defun ,class-name ()
          (make ,class-name))
        (class ,class-name))))

(defmacro generic (name)
  #`(identifier ,name generic))

(defmacro defgeneric (name &rest args)
  #`(progn
      (defvar (generic ,name) (make-generic))
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

;; Fixup class hierarchy

(set-superclass (class <string>) (class object))
(set-superclass (class <boolean>) (class object))
(set-superclass (class <compound-form>) (class object))
(set-superclass (class <number-form>) (class object))
(set-superclass (class <string-form>) (class object))
(set-superclass (class <symbol-form>) (class object))
(set-superclass (class <string-dict>) (class object))
(set-superclass (class <list>) (class object))
(set-superclass (class <function>) (class object))
(set-superclass (class nil) (class object))

;; Numbers
;;
;; small-integer, big-integer, rational, and real are defined in JS.

(defclass number)
(set-superclass (class real) (class number))
(set-superclass (class rational) (class real))
(defclass (integer rational))
(set-superclass (class small-integer) (class integer))
(set-superclass (class big-integer) (class integer))

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

;;

(defmethod = ((a object) (b object))
  (eq a b))

;;

(defmacro dynamic (name)
  #`(identifier ,name dynamic))

(defmacro defdynamic (name &optional (value #'nil))
  #`(defvar (dynamic ,name) ,value))

(defmacro dynamic-bind (bindings &rest body)
  (if (empty? bindings)
      #`(progn ,@body)
      #`(dynamic-bind-1 ,(elt bindings 0)
          (dynamic-bind ,(slice bindings 1)
             ,@body))))

(defmacro dynamic-bind-1 (binding &rest body)
  (let ((name (elt binding 0))
        (value (elt binding 1)))
  #`(let ((old-value (dynamic ,name)))
      (setq (dynamic ,name) ,value)
      (unwind-protect (progn ,@body)
        (setq (dynamic ,name) old-value)))))

;; 

(defgeneric show (object))

;; fallback
(defmethod show ((a object))
  #{ JSON.stringify(~a) #})

(defmethod show ((a nil))
  "nil")

(defmethod show ((a number))
  #{ (~a).toString() #})

(defmethod show ((a <boolean>))
  (if a "#t" "#f"))

(defmethod show ((a <function>))
  "#<function>")


;; Conditions

(defmacro deferror (name &rest slots)
  #`(defclass (,name <error>) ,@slots))

(defclass <exception>)
(defclass (<error> <exception>))
(defclass (<warning> <exception>))
(defclass (<restart> <exception>))

;; Collections

(defgeneric elt (collection index))
(defgeneric empty? (collection))
(defgeneric slice (collection from-index))

(defmethod elt ((c <compound-form>) (i small-integer))
  (compound-elt c i))

(defmethod empty? ((c <compound-form>))
  (compound-empty? c))

(defmethod slice ((c <compound-form>) (from small-integer))
  (compound-slice c from))

(defclass <dict>)

(defgeneric get (dict key &optional default))
(defgeneric put (dict key value))
(defgeneric has-key (dict key))

(defclass (<string-dict> <dict>))

(defmethod get ((dict <string-dict>) (key <string>) &optional default)
  (if (has-key dict key)
      (string-dict-get dict key)
      default))

(defmethod put ((dict <string-dict>) (key <string>) value)
  (string-dict-put dict key value))

(defmethod has-key ((dict <string-dict>) (key <string>))
  (string-dict-has-key dict key))

(defclass <list>)

(defun <list> (&rest args) 
  (apply $list args))

(defmethod iter ((list <list>))
  (<list-iter> list))

(defmethod len ((list <list>))
  (list-len list))

(defmethod elt ((list <list>) (i number))
  (list-elt list i))

(defmethod add ((list <list>) elt)
  (list-add list elt))

(defclass <list-iter> (list i))

(defun <list-iter> ((list <list>))
  (let ((iter (make <list-iter>)))
    (set (.list iter) list)
    (set (.i iter) 0)
    iter))

(defmethod has-next ((iter <list-iter>))
  (< (.i iter) (len (.list iter))))

(defmethod next ((iter <list-iter>))
  (inc (.i iter)))

(defmethod now ((iter <list-iter>))
  (elt (.list iter) (.i iter)))

(defmethod elt ((form <compound-form>) i)
  (compound-elt form i))

(defmethod iter ((form <compound-form>))
  (iter (compound-elts form)))

(defun every ((pred <function>) coll)
  "Returns #t iff every element of a collection satisfies the predicate."
  (block exit
    (let ((iter (iter coll)))
      (while (has-next iter)
        (unless (funcall pred (now iter))
          (return-from exit #f))
        (next iter)))
    #t))

(defun each ((fun <function>) &rest colls)
  "Applies a function to the elements of one or more collections for
effect.  The function is called with N positional arguments, each
taken from the collections from left to right.  The shortest
collection determines how many times the function is called."
  (if (= 1 (len colls))
      (let ((iter (iter (elt colls 0))))
        (while (has-next iter)
          (funcall fun (now iter))
          (next iter)))
      (let ((iters (map $iter colls)))
        (while (every $has-next iters)
          (apply fun (map $now iters))
          (each $next iters)))))

(defun map ((fun <function>) &rest colls &key (into (<list>)))
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
      (let ((iters (map $iter colls)))
        (while (every $has-next iters)
          (add into (apply fun (map $now iters)))
          (each $next iters))))
  into)

(defclass <number-iter> (i max))

(defun <number-iter> (max)
  (let ((iter (make <number-iter>)))
    (set (.i iter) 0)
    (set (.max iter) max)
    iter))

(defmethod iter ((max number))
  (<number-iter> max))

(defmethod has-next ((iter <number-iter>))
  (< (.i iter) (.max iter)))

(defmethod now ((iter <number-iter>))
  (.i iter))

(defmethod next ((iter <number-iter>))
  (inc (.i iter)))

(defmacro dotimes (var-and-ct &rest body)
  (let ((var (elt var-and-ct 0))
        (ct (elt var-and-ct 1)))
    #`(each (lambda (,var) ,@body) ,ct)))
