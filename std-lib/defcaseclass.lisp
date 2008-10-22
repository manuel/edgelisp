;;;;; Case Classes

;;; Scala-like case classes:
;;;
;;;  (defcaseclass <maybe>
;;;    (<just> x)
;;;    (<nothing>))

(defmacro defcaseclass (name @cases)
  `(progn
     (defclass ,name)
     ,@(map (lambda (c) 
              `(defclass (,(first c) ,name) ,@(rest c)))
            cases)))

(defmacro defcaseclass (name @cases)
  (--append (--compound (--symbol "progn")
                        (--compound (--symbol "defclass")
                                    name))
            (map (lambda (c)
                   (--append (--compound (--symbol "defclass")
                                         (--compound (first c) name))
                             (rest c)))
                 cases)))

