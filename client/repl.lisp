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

;;;; Read-Eval-Print Loop

(load "client/dom.lisp")
(load "client/html.lisp")

(defclass retry-repl-request (restart))

(defun repl-eval (form)
  (block end
    (loop
       (block retry
         (restart-bind ((retry-repl-request
                         (lambda (r)
                           (note "Retrying")
                           (return-from retry)))
                        (abort
                         (lambda (r)
                           (note "Aborting")
                           (return-from end))))
           (return-from end (eval form)))))))

(dom-append
 (html-div :id "output")
 (html-form :action "javascript:_lisp_function_repl(null)"
            (html-input :id "input" :type "text")))

(defun print ((a object))
  (dom-append-child (dom-id "output") (html-text (show a))))

(defun repl ()
  (print (repl-eval (read-from-string (dom-value (dom-id "input")))))
  #f)

(provide "repl")
