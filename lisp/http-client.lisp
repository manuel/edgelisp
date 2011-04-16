(defgeneric http-get (url))

(defmethod http-get ((url <string>))
  {% (function(){
       var req = new XMLHttpRequest();
       req.open("GET", ~url, false);
       req.send(null);
       return req.responseText;
      })() %})

(defmacro alien-body (expr)
  `{% function(){ ~,expr } %})
