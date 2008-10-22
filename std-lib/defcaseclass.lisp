;;;;; Case Classes

;;; Scala-like case classes:
;;;
;;;  (defcaseclass <maybe>
;;;    (<just> x)
;;;    (<nothing>))

(defmacro defcaseclass (name @cases)
  `(progn
     (defclass ,name)
     ,@(map (lambda (c) 
              `(defclass (,(first c) ,name) ,@(rest c)))
            cases)))

;; Approximately decoded to:
;;
;; (defmacro defcaseclass (name @cases)
;;   (--append (--compound (--symbol "progn")
;;                         (--compound (--symbol "defclass")
;;                                     name))
;;             (map (lambda (c)
;;                    (--append (--compound (--symbol "defclass")
;;                                          (--compound (first c) name))
;;                              (rest c)))
;;                  cases)))


;; Generated macro function (annotated):
;;
;; function (__lispV_HHlispHform) {
;;     var __lispV_name = __lispF_BC(__lispV_HHlispHform, 0);              // destructure input form
;;     var __lispV_Icases = __lispF_slice(__lispV_HHlispHform, 1);         // 
;;     return lispCall("compound-forms-append", 
;;                     [lispCall("new-compound-form", 
;;                               [lispCall("new-symbol-form", "progn"), 
;;                                lispCall("compound-forms-append", 
;;                                         [lispCall("new-compound-form", 
;;                                                   [lispCall("new-symbol-form", "defclass"), 
;;                                                    __lispV_name])])]),
;;                      __lispF_map(function (__lispV_c) {
;;                              return lispCall("compound-forms-append", 
;;                                              [lispCall("new-compound-form", 
;;                                                        [lispCall("new-symbol-form", "defclass"), 
;;                                                         lispCall("compound-forms-append", 
;;                                                                  [lispCall("new-compound-form", 
;;                                                                            [__lispF_first(__lispV_c), 
;;                                                                             __lispV_name])])]), 
;;                                               __lispF_rest(__lispV_c)]);}, 
;;                          __lispV_cases)]);
;; }
