(defun load ((path string) -> nil)
  (%load path))

(defun alert (&optional (s "Lisp alert"))
  #{ alert(~s) #})

(defun prompt (&optional (s "Lisp prompt"))
  "Returns user-entered string or nil."
  #{ prompt(~s) #})

(defun print ((a object))
  nil)

(defun note ((a object))
  (print (show a)))

(provide "client-boot")
