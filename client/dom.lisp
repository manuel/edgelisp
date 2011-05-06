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

(defun dom-document () #{ document #})

(defun dom-document-body () #{ document.body #})

(defun dom-append-child (parent element)
  #{ (~parent).appendChild((~element)), null #})

(defun dom-create-element ((tag string))
  #{ ~(dom-document).createElement((~tag)) #})

(defun dom-set-id (element (id string))
  #{ (~element).id = (~id) #})

(defun dom-set-inner-html (element (html string))
  #{ (~element).innerHTML = (~html) #})

(provide "dom")
