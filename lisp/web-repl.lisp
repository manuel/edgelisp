(defun web-repl-eval (form)
  (block toplevel
    (handler-bind ((return-to-toplevel
                    (lambda (r) (return-from toplevel))))
      (eval form))))

