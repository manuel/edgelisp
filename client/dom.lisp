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

;;;; Document Object Model

(defun dom-document (-> native) #{ document #})

(defun dom-document-body (-> native) #{ document.body #})

(defun dom-append-child ((parent native) (element native) -> nil)
  #{ (~parent).appendChild((~element)), null #})

(defun dom-create-element ((tag string) -> native)
  #{ ~(dom-document).createElement((~tag)) #})

(defun dom-set-attribute ((element native) (attr-name string) (value object) -> nil)
  #{ (~element).setAttribute((~attr-name), (~value)), null #})

(defun dom-set-id ((element native) (id string) -> nil)
  #{ (~element).id = (~id), null #})

(defun dom-set-inner-html ((element native) (html string) -> nil)
  #{ (~element).innerHTML = (~html), null #})


(defun dom-append (&rest elements -> nil)
  (each (lambda (element)
          (dom-append-child (dom-document-body) element))
        elements))

(provide "dom")
