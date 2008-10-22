(defclass <true>)
(defvar true (<true>))

(defclass <false>)
(defvar false (<false>))

(def not (<true>) false)
(def not (<false>) true)

(defun naturalize-jsbool (jsbool)
  {% ~(jsbool) ? ~(true) : ~(false) %})


(defun == (a b)
  (naturalize-jsbool {% a == b %}))

(defun != (a b)
  (not (== a b)))


(defun if-fn (test-fn then-fn else-fn)
  {% ~(!= (apply test-fn) false) ? ~((apply then-fn)) : ~((apply else-fn)) %})

(defun while-fn (test-fn fn)
  {% while(~((!= false (apply test-fn)))) { ~((apply fn)) } %})
  

(defun map (fn coll)
  (let ((clone (new (type-for-copy coll)))
        (iterator (iterator coll)))
    (while-fn (lambda () (not (done iterator)))
      (lambda ()
        (set ([] clone (index iterator)) (apply fn (current iterator)))
        (set iterator (next iterator))))
    clone))

(defun first (coll)
  ([] coll 0))

(defun second (coll)
  ([] coll 1))
