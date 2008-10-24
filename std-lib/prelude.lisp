(defclass <true>)
(def show (<true>) "true")
(defvar true (<true>))

(defclass <false>)
(def show (<false>) "false")
(defvar false (<false>))

(def not (<true>) false)
(def not (<false>) true)

(defun jsbool-naturalize (jsbool) 
  {% ~(jsbool) ? ~(true) : ~(false) %})

(defun == (a b) 
  (naturalize-jsbool {% ~(a) == ~(b) %}))

(defun != (a b) 
  (not (== a b)))

(defun if-fn (test-fn then-fn else-fn) 
  {% ~((!= (apply test-fn) false)) ? ~((apply then-fn)) : ~((apply else-fn)) %})

(defun while-fn (test-fn fn)
  {% lispWhile(~(test-fn), ~(fn)) %})

(defun map (fn coll)
  (let ((clone (empty-clone coll))
        (iterator (iterator coll)))
    (while-fn (lambda () (not (done iterator)))  
              (lambda ()       
                (set ([] clone (index iterator)) (apply fn (current iterator)))  
                (set iterator (next iterator))))
    clone))

(defun for-each (fn coll) 
  (let ((iterator (iterator coll)))   
    (while-fn (lambda () (not (done iterator)))   
              (lambda ()             
                (apply fn (current iterator))    
                (set iterator (next iterator))))))

(defun first (coll) 
  ([] coll {% 0 %})) 

(defun second (coll) 
  ([] coll {% 1 %})) 

(defun rest (coll)
  (slice coll {% 1 %}))
