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

(defun load ((path string) -> nil)
  (%load path))

(defun alert (&optional (s "Lisp alert"))
  #{ alert(~s) #})

(defun prompt (&optional (s "Lisp prompt"))
  "Returns user-entered string or nil."
  #{ prompt(~s) #})

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

(defun dom-set-attribute ((element native) (attr-name string) (value object) -> nil)
  #{ (~element).setAttribute((~attr-name), (~value)), null #})

(defun dom-value ((element native) -> object)
  #{ (~element).value #})

(defun dom-set-value ((element native) (s string) -> nil)
  #{ (~element).value = (~s), null #})

(defun dom-focus ((element native))
  #{ (~element).focus(), null #})

(defun dom-set-id ((element native) (id string) -> nil)
  #{ (~element).id = (~id), null #})

(defun dom-set-inner-html ((element native) (html string) -> nil)
  #{ (~element).innerHTML = (~html), null #})


(defun dom-append (&rest elements -> nil)
  (each (lambda (element)
          (dom-append-child (dom-document-body) element))
        elements))

(defun dom-text ((s string) -> native)
  #{ document.createTextNode((~s)) #})

;;; Scripts

(defun dom-load-script ((url string) &optional (type "text/javascript"))
  (dom-append (html-script :src url :type type)))

;;;; Hypertext Markup Language

(defmacro define-html-tag (name &optional (prefix #'html-))
  (let ((fun-name (string-to-identifier 
                   (string-concat (identifier-name prefix) name))))
    #`(defun ,fun-name (&all-keys attrs &rest children)
        (let ((element (dom-create-element ,(string-to-form (identifier-name name)))))
          (string-dict-map (lambda (attr-name)
                             (dom-set-attribute element attr-name (get attrs attr-name)))
                           attrs)
          (each (lambda (child)
                  (dom-append-child element child))
                children)
          element))))

(define-html-tag button)
(define-html-tag div)
(define-html-tag form)
(define-html-tag input)
(define-html-tag script)
(define-html-tag span)

(defun html-text ((s string) -> native)
  (dom-text s))

;;;; Hypertext Transfer Protocol

(defun http-get ((url string) -> string)
  (native-body 
   #{ var req = new XMLHttpRequest();
      req.open("GET", ~url, false);
      req.send(null);
      return req.responseText #}))

(provide "client-boot")
