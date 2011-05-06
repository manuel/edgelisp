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

;;;; Hypertext Markup Language

(defun html-div (&key id &rest children)
  (let ((element (dom-create-element "div")))
    (when id
      (dom-set-id element id))
    (each (lambda (child)
            (dom-append-child element child))
          children)
    element))
    
(defun html-text ((s string))
  (let ((element (dom-create-element "span")))
    (dom-set-inner-html element s)
    element))

(provide "html")
