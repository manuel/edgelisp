(defclass retry-repl-request (restart))

(defun web-repl-eval (form)
  (block abort
    (loop
       (block retry
         (restart-bind ((retry-repl-request (lambda (r)
                                 (note "Retrying")
                                 (return-from retry))
                          nil)
                        (abort (lambda (r)
                                 (note "Aborting")
                                 (return-from abort))
                          nil)
                        (hard-abort (lambda (r)
                                      (hard-abort))
                          nil))
           (eval form))))))

(provide "web-repl")
