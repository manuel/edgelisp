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

(defun repl ()
  (let* ((input (dom-id "input"))
         (value (.element-value input)))
    (repl-history-add value)
    (dynamic-bind ((print-readably #f))
      (print value))
    (print (repl-eval (read-unit-as-progn value)))
    (setf (.element-value input) "")))

(defun repl-scroll-to-bottom ()
  (let ((e (window-element *repl-window*)))
    (setf (.element-scroll-top e) (.element-scroll-height e))))

(unless (and (defined? \local-storage-supported?)
             (local-storage-supported?))
  (warn "Local storage not supported"))

;;;; History

(defvar *repl-offset* 0)

(defun repl-history-count (-> number)
  (if-option (ct-str (local-storage-get-item "repl-history-count"))
    (eval (read-from-string ct-str))
    0))

(defun repl-history-item-name ((ct number) -> string)
  (string-concat "repl-history-item-" (show ct)))

(defun repl-history-add ((input string))
  (when (and (defined? \local-storage-supported?)
             (local-storage-supported?))
    (let ((ct (repl-history-count)))
      (local-storage-set-item "repl-history-count" (show (+ 1 ct)))
      (local-storage-set-item (repl-history-item-name ct)
                              input)
      (setq *repl-offset* 0))))

(defun repl-history-previous-item ()
  (when (and (defined? \local-storage-supported?)
             (local-storage-supported?))
    (when (< *repl-offset* (repl-history-count))
      (incf *repl-offset*)
      (let ((i (- (repl-history-count) *repl-offset*)))
        (if-option (item (local-storage-get-item (repl-history-item-name i)))
          (setf (.element-value (dom-id "input")) item))))))

(defun repl-history-next-item ()
  (when (and (defined? \local-storage-supported?)
             (local-storage-supported?))
    (decf *repl-offset*)
    (let ((i (- (repl-history-count) *repl-offset*)))
      (when (and (> i -1) (< i (repl-history-count)))
        (if-option (item (local-storage-get-item (repl-history-item-name i)))
          (setf (.element-value (dom-id "input")) item))))))

(defclass repl-mode (mode))
(defun make-repl-mode (-> repl-mode)
  (make repl-mode))
(defmethod mode-init-window ((m repl-mode) (w window))
  (set-window-title w "REPL")
  (dom-append-child
   (window-element w)
   (<div>
    (<div> :data-wid "output")
    (<form> :onsubmit "try{_lisp_function_repl(null);}finally{return false;}"
            (let ((input (<input> :id "input" :type "text" :autocomplete "off")))
              (dom-keydown input \repl-input-keydown)
              input)))))

(defun repl-input-keydown ((event native))
  (if (= (.event-which event) +key-up-arrow+)
      (repl-history-previous-item)
      (if (= (.event-which event) +key-down-arrow+)
          (repl-history-next-item))))

(defvar *repl-window* (make-window (dynamic window-manager)
                                   nil
                                   :mode (make-repl-mode)))

(defun print ((a object))
  (dom-append-child (window-find *repl-window* "output")
                    (<div> (markup a)))
  (repl-scroll-to-bottom))

(dom-focus (window-find *repl-window* "output"))
(note "EdgeLisp 0.1.19 loaded")
