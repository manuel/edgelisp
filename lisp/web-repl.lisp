(defun web-repl-eval (form)
  (block abort
    (restart-bind ((abort (lambda (r)
                            (print "Evaluation aborted")
                            (return-from abort))
                     nil))
      (eval form))))
