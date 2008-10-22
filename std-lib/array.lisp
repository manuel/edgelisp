;;; Wraps a JS array ("peer")

(defclass <array> peer)

(defun new-array ()
  (<array .peer {% [] %}>))

(def type-for-copy (<array>)
  <array>)

(def [] (<array .peer> i)
  {% ~(peer)[~(i)] %})

(def []-setter (<array .peer> i v)
  {% ~(peer)[~(i)] = ~(v) %})

(def slice (<array .peer> start)
  (<array .peer {% ~(peer).slice(~(start)) %}>))

(defclass <array-iterator> array i)

(def iterator (<array>) 
  (<array-iterator> .array array .i {% 0 %}))

(def done (<array-iterator .array .i>)
  (jsbool-naturalize {% ~(i) >= (~((.peer array)).length - 1) %}))

(def current (<array-iterator .array .i>) 
  ([] array i))

(def next (<array-iterator .iter .i>)
  (set (.i iter) {% ~(i) + 1 %}) 
  iter)

(def index (<array-iterator .i>) 
  {% ~(i) %})
