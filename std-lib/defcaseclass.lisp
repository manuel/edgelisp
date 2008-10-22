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
