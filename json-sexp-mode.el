;;; json-sexp-mode.el --- Edit JSON in s-expression form.

;; Copyright (C) 2013  Taylan Ulrich B.

;; Author: Taylan Ulrich B. <taylanbayirli@gmail.com>
;; Keywords: data, files

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Depends on a version of json.el that supports pretty-printing.
;; Recent Emacs versions have this, as of 2013-03.

;;; Code:
(require 'json)

(defun json-sexp-convert-buffer-to-sexp ()
  (unless (zerop (buffer-size))
    (let ((data (let ((json-object-type 'plist))
                  (json-read-from-string (buffer-string)))))
      (erase-buffer)
      (insert (pp-to-string data)))))

(defun json-sexp-convert-buffer-to-json ()
  (unless (zerop (buffer-size))
    (let ((data (car (read-from-string (buffer-string)))))
      (erase-buffer)
      (insert (let ((json-encoding-pretty-print t))
                (json-encode data))))))

(define-derived-mode json-sexp-mode emacs-lisp-mode "JSON-sexp"
  "Major mode for editing JSON in s-expression form.
The buffer-contents, which must be JSON, are transformed to
s-expressions when this mode is started, and transformed back
temporarily to JSON whenever the buffer is saved."
  (json-sexp-convert-buffer-to-sexp)
  (set-buffer-modified-p nil)
  (add-hook 'before-save-hook 'json-sexp-convert-buffer-to-json nil t)
  (add-hook 'after-save-hook 'json-sexp-after-save nil t))

(defun json-sexp-after-save ()
  (json-sexp-convert-buffer-to-sexp)
  (set-buffer-modified-p nil))

(provide 'json-sexp-mode)
;;; json-sexp-mode.el ends here
