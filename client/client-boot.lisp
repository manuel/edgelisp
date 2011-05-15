; EdgeLisp: A Lisp that compiles to JavaScript.
; Copyright (C) 2008-2011 by Manuel Simoni.
;
; This program is free software: you can redistribute it and/or modify
; it under the terms of the GNU Affero General Public License as
; published by the Free Software Foundation, version 3.
;
; This program is distributed in the hope that it will be useful,
; but WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
; GNU Affero General Public License for more details.
;
; You should have received a copy of the GNU Affero General Public License
; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; This file contains browser-related functionality, and is loaded
;;; after lisp/boot.lisp.

(defun note ((a object))
  (dynamic-bind ((print-readably #f))
    (print (string-concat "; " (show a)))))

(defun compile-file ((path string) -> fasl)
  (%compile-file path))

(defun load-file ((path string) -> nil)
  (%load-file path))

(defmethod load ((what string))
  (load-file what))

(defun alert (&optional (s "Lisp alert"))
  #{ alert(~s) #})

(defun prompt (&optional (s "Lisp prompt"))
  "Returns user-entered string or nil."
  #{ prompt(~s) #})

(defun read-unit-as-progn ((s string))
  (%read-unit-as-progn s))

;;;; Document Object Model

;;; Document Elements

(defun dom-document (-> native) #{ document #})

(defun dom-document-body (-> native) #{ document.body #})

(defun dom-id ((id string) -> object)
  #{ document.getElementById((~id)) #})

(defun dom-append-child ((parent native) (element native) -> nil)
  #{ (~parent).appendChild((~element)), null #})

(defun dom-create-element ((tag string) -> native)
  #{ document.createElement((~tag)) #})

(defun dom-focus ((element native))
  #{ (~element).focus(), null #})

(defun dom-append (&rest elements -> nil)
  (each (lambda (element)
          (dom-append-child (dom-document-body) element))
        elements))

(defun dom-text ((s string) -> native)
  #{ document.createTextNode((~s)) #})

(define-native-slot element-id "id")
(define-native-slot element-inner-html "innerHTML")
(define-native-slot element-scroll-height "scrollHeight")
(define-native-slot element-scroll-top "scrollTop")
(define-native-slot element-value "value")
(define-native-slot event-which "which")
(define-native-slot event-target "target")
(define-native-slot node-value "nodeValue")

(defun dom-set-attribute ((element native) (attr-name string) (value object) -> nil)
  #{ (~element).setAttribute((~attr-name), (~value)), null #})

(defun dom-get-attribute ((element native) (attr-name string) -> object)
  #{ (~element).getAttribute((~attr-name)) #})

(defun dom-set-data ((element native) (attr-name string) (value object) -> nil)
  #{ $(~element).data((~attr-name), (~value)), null #})

(defun dom-get-data ((element native) (attr-name string) -> object)
  #{ $(~element).data((~attr-name)) #})

;;; Scripts

(defun dom-load-script ((url string) &optional (type "text/javascript"))
  (dom-append (<script> :src url :type type)))

;;; Events

(defun dom-keydown ((element native) (callback function))
  #{ $(~element).keydown(~(native-callback callback)), null #})

(defun dom-onclick ((element native) (callback function))
  #{ $(~element).click(~(native-callback callback)), null #})

;;;; Hypertext Markup Language

(defmacro define-markup-tag (name)
  "Given unevaluated identifier NAME, constructs a function called
  <NAME> that constructs a DOM element with the NAME as tag.  Keyword
  arguments to the function are attached as DOM attributes."
  (let ((fun-name (string-to-identifier 
                   (string-concat "<" name ">"))))
    #`(defun ,fun-name (&all-keys attrs &rest children)
        (let ((element (dom-create-element ,(string-to-form (identifier-name name)))))
          (string-dict-map (lambda (attr-name)
                             (dom-set-attribute element attr-name (get attrs attr-name)))
                           attrs)
          (each (lambda (child)
                  (dom-append-child element (markup child)))
                children)
          element))))

(define-markup-tag button)
(define-markup-tag div)
(define-markup-tag em)
(define-markup-tag form)
(define-markup-tag input)
(define-markup-tag li)
(define-markup-tag ol)
(define-markup-tag script)
(define-markup-tag span)
(define-markup-tag strong)
(define-markup-tag ul)

(defgeneric markup (object -> native)
  (:documentation "Returns DOM element for object, analogous to string
  for SHOW"))
(defmethod markup ((o object) -> native)
  (markup (show o)))
(defmethod markup ((s string) -> native)
  (dom-text s))
(defmethod markup ((n native) -> native)
  n)

;;;; Hypertext Transfer Protocol

(defun http-get ((url string) -> string)
  (native-body 
   #{ var req = new XMLHttpRequest();
      req.open("GET", ~url, false);
      req.send(null);
      return req.responseText #}))

;;;; Browser

(defvar +key-left-arrow+ 37)
(defvar +key-up-arrow+ 38)
(defvar +key-right-arrow+ 39)
(defvar +key-down-arrow+ 40)

(defun window-open ((url string))
  #{ window.open(~url), null #})

(provide "client-boot")
