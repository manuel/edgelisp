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

(defun print ((a object))
  (dom-append-child (dom-id "output") (html-text (show a)))
  (repl-scroll-to-bottom))

(defun repl ()
  (let* ((input (dom-id "input"))
         (value (dom-value input)))
    (dynamic-bind ((print-readably #f))
      (note value))
    (print (repl-eval (read-from-string value)))
    (dom-set-value input "")
    (repl-history-add value))
  #f)

(defun repl-scroll-to-bottom ()
  #{ window.scrollTo(0, document.body.scrollHeight), null #})

(dom-append
 (html-div :id "output")
 (html-form :action "javascript:_lisp_function_repl(null)"
            (html-input :id "input" :type "text")))

(dom-focus (dom-id "input"))
(note "EdgeLisp 0.1.13")

(unless (and (defined? \local-storage-supported?)
             (local-storage-supported?))
  (warn "Local storage not supported"))

;;;; History

(defun repl-history-add ((input string))
  (when (and (defined? \local-storage-supported?)
             (local-storage-supported?))
    (let ((ct-str (local-storage-get-item "repl-history-count"))
          (ct 0))
      (when ct-str
        (setq ct (eval (read-from-string ct-str))))
      (local-storage-set-item "repl-history-count" (show (+ 1 ct)))
      (local-storage-set-item (string-concat "repl-history-item-" (show ct))
                              input))))

