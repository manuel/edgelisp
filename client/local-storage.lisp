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

;;;; HTML5 Local Storage

;;; Thanks to Mark Pilgrim for docs
;;; http://diveintohtml5.org/storage.html

(defclass local-storage-error (error))

(defun local-storage-supported? (-> boolean)
  "Returns true if local storage is supported, false otherwise."
  (native-body #{
    try {
      return "localStorage" in window && window["localStorage"] !== null;
    } catch (e) {
      return false;
    }
    #}))

(defun local-storage-get-item ((key string) -> string)
  "Returns the local storage data associated with key, or nil."
  (unless (local-storage-supported?)
    (error (make local-storage-error)))
  #{ localStorage.getItem((~key)) #})

(defun local-storage-set-item ((key string) (data string) -> nil)
  "Updates the local storage data associated with key."
  (unless (local-storage-supported?)
    (error (make local-storage-error)))
  #{ localStorage.setItem((~key), (~data)) #}
  nil)

(defun local-storage-remove-item ((key string) -> nil)
  "Deletes the local storage data associated with key."
  (unless (local-storage-supported?)
    (error (make local-storage-error)))
  #{ localStorage.removeItem((~key)) #}
  nil)

(defun local-storage-length (-> small-integer)
  "Returns the number of local storage items."
  (unless (local-storage-supported?)
    (error (make local-storage-error)))
  #{ localStorage.length #})

(defun local-storage-key ((i small-integer) -> string)
  "Returns the key of the local storage item with the given index."
  (unless (local-storage-supported?)
    (error (make local-storage-error)))
  #{ localStorage.key((~i)) #})

(provide "local-storage")
