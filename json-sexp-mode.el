;;; json-sexp-mode.el --- Edit JSON in s-expression form.

;; Copyright (C) 2013 - 2015  Taylan Ulrich Bayırlı/Kammer

;; Author: Taylan Ulrich Bayırlı/Kammer <taylanbayirli@gmail.com>
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

;; Depends on a version of json.el that supports the `json-nil' parameter, such
;; as the one in this repository.

;;; Code:
(require 'jsxp)

(defun json-sexp-convert-region-to-sexp (start end)
  "Convert region from JSON to sexps."
  (interactive "r")
  (unless (= start end)
    (let ((data (jsxp-read-from-string (buffer-substring start end)))
          (point (point)))
      (delete-region start end)
      (goto-char start)
      (json-sexp--pp data)
      (goto-char point))))

(defun json-sexp-convert-region-to-json (start end)
  "Convert region from sexps to JSON."
  (interactive "r")
  (unless (= start end)
    (let ((data (car (read-from-string (buffer-substring start end))))
          (point (point)))
      (delete-region start end)
      (goto-char start)
      (insert (jsxp-encode data))
      (insert "\n")
      (goto-char point))))

(defvar json-sexp--indent 0)

(defun json-sexp--pp (data)
  "Fast, specialized `pp' implementation."
  (let ((json-sexp--indent (current-indentation)))
    (json-sexp--pp-obj data)))

(defun json-sexp--pp-obj (obj)
  (cond
   ((consp obj)
    (insert "(")
    (let ((json-sexp--indent (+ json-sexp--indent 1)))
      (json-sexp--pp-list obj))
    (insert ")"))
   ((vectorp obj)
    (insert "[")
    (let ((json-sexp--indent (+ json-sexp--indent 1)))
      (json-sexp--pp-vector obj))
    (insert "]"))
   (t
    (prin1 obj (current-buffer)))))

(defun json-sexp--pp-list (list)
  (let ((split (catch 'break
                 (dolist (obj list)
                   (if (or (consp obj)
                           (vectorp obj))
                       (throw 'break t)))
                 nil)))
    (while list
      (json-sexp--pp-obj (car list))
      (setq list (cdr list))
      (unless (null list)
        (if split
            (progn
              (insert "\n")
              (insert-char ?\s json-sexp--indent))
          (insert " "))))))

(defun json-sexp--pp-vector (vector)
  (let ((split (catch 'break
                 (let ((i 0))
                   (while (< i (length vector))
                     (if (or (consp (aref vector i))
                             (vectorp (aref vector i)))
                         (throw 'break t))
                     (setq i (+ i 1)))
                   nil))))
    (let ((i 0))
      (while (< i (length vector))
        (json-sexp--pp-obj (aref vector i))
        (setq i (+ i 1))
        (unless (= i (length vector))
          (if split
              (progn
                (insert "\n")
                (insert-char ?\s json-sexp--indent))
            (insert " ")))))))

(defun json-sexp-convert-buffer-to-sexp ()
  "Convert buffer from JSON to sexps."
  (interactive)
  (json-sexp-convert-region-to-sexp (point-min) (point-max)))

(defun json-sexp-convert-buffer-to-json ()
  "Convert buffer from sexps to JSON."
  (interactive)
  (json-sexp-convert-region-to-json (point-min) (point-max)))

(define-derived-mode json-sexp-mode emacs-lisp-mode "JSON-sexp"
  "Major mode for editing JSON in s-expression form.
The buffer-contents, which must be JSON, are transformed to
s-expressions when this mode is started, and transformed back
temporarily to JSON whenever the buffer is saved."
  (let ((was-modified (buffer-modified-p)))
    (json-sexp-convert-buffer-to-sexp)
    (set-buffer-modified-p was-modified))
  (add-hook 'before-save-hook 'json-sexp-convert-buffer-to-json nil t)
  (add-hook 'after-save-hook 'json-sexp-after-save nil t))

(defun json-sexp-after-save ()
  (json-sexp-convert-buffer-to-sexp)
  (set-buffer-modified-p nil))

(provide 'json-sexp-mode)
;;; json-sexp-mode.el ends here
