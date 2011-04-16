;; 

(%%defsyntax defmacro
  (%%lambda (defmacro-form)
    `(%%defsyntax ,(compound-elt defmacro-form 1)
       (%%lambda (%%form)
         (compound-apply
           (%%lambda ,(compound-elt defmacro-form 2)
             (progn
               ,@(compound-slice defmacro-form 3)))
           (compound-slice %%form 1))))))

(defmacro identifier (name namespace) `(%%identifier ,name ,namespace))
(defmacro defined? (name) `(%%defined? ,name))
(defmacro fdefined? (name) `(defined? (%%identifier ,name f)))
(defmacro lambda (sig &rest body) `(%%lambda ,sig (progn ,@body)))
(defmacro function (name) `(%%identifier ,name f))
(defmacro defparameter (name value) `(%%defparameter ,name ,value))

(defmacro if (test consequent &opt (alternative 'null))
  `(%%if ,test ,consequent ,alternative))

(defmacro defun (name sig &rest body)
  `(defparameter (function ,name) (lambda ,sig ,@body)))

(defmacro defvar (name value)
  `(defparameter ,name (if (defined? ,name) ,name ,value)))

(defmacro when (test &rest consequent)
  `(if ,test (progn ,@consequent) null))

(defmacro unless (test &rest alternative)
  `(if ,test null (progn ,@alternative)))

(defmacro let (bindings &rest body)
  `(funcall (lambda ,(compound-map (lambda (b) 
                                     (compound-elt b 0))
                                   bindings)
              ,@body)
            ,@(compound-map (lambda (b) 
                              (compound-elt b 1)) 
                            bindings)))

(defmacro let* (vs &rest forms)
  (if (> (compound-len vs) 0)
      `(let (,(compound-elt vs 0)) (let* ,(compound-slice vs 1) ,@forms))
      `(let () ,@forms)))

(defmacro while (test &rest body)
  `(call-while (lambda () ,test) (lambda () ,@body)))

(defmacro block (name &rest body)
  `(call-with-escape-function (lambda (,name) ,@body)))

(defmacro return-from (name &opt (value 'null))
  `(funcall ,name ,value))

(defmacro unwind-protect (protected &rest cleanups)
  `(call-unwind-protected (lambda () ,protected)
                          (lambda () ,@cleanups)))

(defmacro handle (handler-specs &rest body)
  `(bind-handlers (list ,@(compound-map (lambda (handler-spec)
                                          `(list ,(compound-elt handler-spec 0)
                                                 ,(compound-elt handler-spec 1)))
                                        handler-specs))
                  (lambda () ,@body)))

(defmacro eval-when-compile (&rest forms)
  `(%%eval-when-compile (progn ,@forms)))

(defmacro eval-and-compile (&rest forms)
  `(progn
     (eval-when-compile ,@forms)
     ,@forms))

(defun not (x)
  (if x false true))

(eval-when-compile
  (defun setter-name (getter-name)
    (string-concat getter-name "-setter")))

(defmacro set (place value)
  (if (symbol? place)
      `(%%set ,place ,value)
      `(,(string-to-symbol (setter-name (symbol-name (compound-elt place 0))))
        ,@(compound-slice place 1)
        ,value)))

(defmacro inc (place &opt (delta '1))
  `(set ,place (+ ,place ,delta)))

(defmacro dec (place &opt (delta '1))
  `(set ,place (- ,place ,delta)))

(defmacro assert (test &opt (msg '"assertion failed"))
  `(let ((result ,test))
     (if result
         result
         (progn
           (print ,msg)
           (print ',test)))))

(defmacro assert-eq (a b)
  `(assert (eq ,a ,b)))

(defmacro assert-eql (a b)
  `(assert (eql ,a ,b)))

(eval-when-compile
  (defun defclass-do-slot (class-name slot)
    (let* ((slot-name (symbol-name slot))
           (slot-name-form (string-to-form slot-name))
           (getter-name (string-concat "." slot-name))
           (setter-name (setter-name getter-name)))
      `(progn
         (defmethod ,(string-to-symbol getter-name) ((obj ,class-name))
           (slot obj ,slot-name-form))
         (defmethod ,(string-to-symbol setter-name) ((obj ,class-name) value)
           (set-slot obj ,slot-name-form value))))))

(defmacro defclass (name-and-super &opt (slots '()))
  (let ((class-name null)
        (superclass null))
    (if (symbol? name-and-super)
        (progn (set class-name name-and-super)
               (set superclass null))
        (progn (set class-name (compound-elt name-and-super 0))
               (set superclass (compound-elt name-and-super 1))))
    `(progn
       (when (not (defined? ,class-name)) 
         (set ,class-name (make-class)))
       ,(if superclass 
            `(set-superclass ,class-name ,superclass) 
            `null)
       ,@(compound-map (lambda (slot) (defclass-do-slot class-name slot)) 
                       slots)
       (defun ,class-name ()
         (make ,class-name))
       ,class-name)))

(defmacro defmethod (name params &rest body)
  (let* ((param (compound-elt params 0))
         (class (if (compound? param)
                    (compound-elt param 1)
                    (if (symbol? param)
                        param
                        (assert false))))
         (name-string (string-to-form (symbol-name name))))
    `(progn
       (set-method ,class ,name-string (lambda ,params ,@body))
       (defgeneric ,name))))

(defmacro defgeneric (name &rest args)
  (let ((name-string (string-to-form (symbol-name name))))
    `(defun ,name (&fast fast-arguments)
       (let ((obj (list-elt fast-arguments 1)))
         (fast-apply (get-method obj ,name-string) fast-arguments)))))

(defmacro deferror (name &rest slots)
  `(defclass (,name <error>) ,@slots))

(defclass <object>)

(defclass <exception>)
(defclass (<error> <exception>))
(defclass (<warning> <exception>))
(defclass (<restart> <exception>))

(deferror <failed-assertion>)

(defclass <dict>)

(defgeneric get (dict key &opt default))
(defgeneric put (dict key value))
(defgeneric has-key (dict key))

(defclass (<string-dict> <dict>))

(defmethod get ((dict <string-dict>) (key <string>) &opt default)
  (if (has-key dict key)
      (string-dict-get dict key)
      default))

(defmethod put ((dict <string-dict>) (key <string>) value)
  (string-dict-put dict key value))

(defmethod has-key ((dict <string-dict>) (key <string>))
  (string-dict-has-key dict key))

(defclass <list>)

(defun <list> (&rest args) 
  (apply #'list args))

(defmethod iter ((list <list>))
  (<list-iter> list))

(defmethod len ((list <list>))
  (list-len list))

(defmethod elt ((list <list>) (i <number>))
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
  "Returns true iff every element of a collection satisfies the predicate."
  (block exit
    (let ((iter (iter coll)))
      (while (has-next iter)
        (unless (funcall pred (now iter))
          (return-from exit false))
        (next iter)))
    true))

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
      (let ((iters (map #'iter colls)))
        (while (every #'has-next iters)
          (apply fun (map #'now iters))
          (each #'next iters)))))

(defun map ((fun <function>) &rest colls &key (into (<list>)))
  "Applies a function to the elements of one or more collections and
returns a collection with the results of each application.  The
function is called with N positional arguments, each taken from the
collections from left to right.  The shortest collection determines
how many times the function is called.  The `into' keyword argument
can be used to supply a different collection to hold the results."
  (if (= 1 (len colls))
      (let ((iter (iter (elt colls 0))))
        (while (has-next iter)
          (add into (funcall fun (now iter)))
          (next iter)))
      (let ((iters (map #'iter colls)))
        (while (every #'has-next iters)
          (add into (apply fun (map #'now iters)))
          (each #'next iters))))
  into)

(defclass <number-iter> (i max))

(defun <number-iter> (max)
  (let ((iter (make <number-iter>)))
    (set (.i iter) 0)
    (set (.max iter) max)
    iter))

(defmethod iter ((max <number>))
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
    `(each (lambda (,var) ,@body) ,ct)))
