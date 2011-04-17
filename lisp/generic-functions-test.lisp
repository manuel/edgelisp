(defclass rock)
(defclass scissors)
(defclass paper)

(defgeneric wins? (thing1 thing2))

(defmethod wins? ((a object) (b object)) false)

(defmethod wins? ((r rock) (s scissors)) true)
(defmethod wins? ((s scissors) (p paper)) true)
(defmethod wins? ((p paper) (r rock)) true)
