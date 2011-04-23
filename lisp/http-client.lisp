(defun http-get ((url string))
  (native-body 
   #{ var req = new XMLHttpRequest();
      req.open("GET", ~url, false);
      req.send(null);
      return req.responseText #}))
