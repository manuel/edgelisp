;;; CASE

(defmacro case (keyform &rest clauses)
  #`(let ((key ,keyform))
      (cond ,@(map (lambda (clause)
                     #`((or ,@(let ((testform (elt clause 0)))
                                (if (compound? testform)
                                    (map (lambda (testkey)
                                           #`(= key ,testkey))
                                         testform)
                                  (list testform))))
                        (progn ,@(slice clause 1))))
                   clauses))))

(defvar else #t)

(defclass case-error (error)
  (datum
   expected-cases))

(defun make-case-error ((datum object) (expected-cases compound-form)
                        -> case-error)
  (make case-error :datum datum :expected-cases expected-cases))

(defmacro ecase (keyform &rest clauses)
  #`(let ((key ,keyform))
      (case key
            ,@clauses 
            (else (error (make-case-error key #'(,@(map (lambda (clause)
                                                          (elt clause 0))
                                                        clauses))))))))

