(defmacro defcaseclass (name @cases)    
  `(progn    
     ,@(map (lambda (c)  
              `(defclass (,(first c) ,name) ,@(rest c)))  
            cases)))
