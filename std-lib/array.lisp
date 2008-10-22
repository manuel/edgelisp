(defclass <array> peer)

(defun new-array () 
  (<array .peer {% [] %}>))

(def type-for-copy (<array>) 
  <array>)

(def []-setter (<array> i v) 
  {% ~((.peer array))[~(i)] = ~(v) %})

(defclass <array-iterator> array i)

(def iterator (<array>) 
  (<array-iterator> .array array .i {% 0 %}))

(def done (<array-iterator .array .i>)
  (jsbool-naturalize {% ~(i) >= (~((.peer array)).length - 1) %}))

(def current (<array-iterator .array .i>) 
  {% ~(array)[~(i)] %})

(def next (<array-iterator .iter .i>)
  (set (.i iter) {% ~(i) + 1 %}) 
  iter)

(def index (<array-iterator .i>) 
  {% ~(i) %})
