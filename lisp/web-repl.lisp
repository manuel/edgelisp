(defun web-repl-eval (form)
  (block abort
    (restart-bind ((abort (lambda (r)
                            (note "Evaluation aborted")
                            (return-from abort))
                     nil))
      (eval form))))

(provide "web-repl")
