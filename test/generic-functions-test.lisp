(defclass rock)
(defclass scissors)
(defclass paper)

(defgeneric wins? (thing1 thing2))

(defmethod wins? ((a object) (b object)) #f)
(defmethod wins? ((r rock) (s scissors)) #t)
(defmethod wins? ((s scissors) (p paper)) #t)
(defmethod wins? ((p paper) (r rock)) #t)
