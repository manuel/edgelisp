(defun web-repl-eval (form)
  (block abort
    (restart-bind ((abort (lambda (r) (return-from abort))
                     nil))
      (eval form))))
