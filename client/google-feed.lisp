(defdynamic google-feed-api-key
  "ABQIAAAAsDylyHuLqoMVDQ-FI1o2rRQIuhHBNtSzOwBJ-bNgzgP7yqq0KBSIT9w56_5MKceXF5GR3HiVt6h9uQ")

(defun google-feed-api-script-url ()
  (string-concat "https://www.google.com/jsapi?key=" (dynamic google-feed-api-key)))

(dom-load-script (google-feed-api-script-url))

(provide "google-feed")
