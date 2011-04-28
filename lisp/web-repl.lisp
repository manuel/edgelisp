(defun web-repl-eval (form)
  (block abort
    (handler-bind ((abort
                    (lambda (r) (return-from abort))))
      (eval form))))
