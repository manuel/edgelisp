(defun google-feed-load ((url string) (callback function))
  #{ new google.feeds.Feed(~url).load(~(native-callback callback)) #})

(provide "google-feed")
