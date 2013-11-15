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

(defun json-sexp-convert-region-to-sexp (start end)
  "Convert region from JSON to sexps."
  (interactive "r")
  (unless (= start end)
    (let ((data (let ((json-object-type 'plist))
                  (json-read-from-string (buffer-substring start end)))))
      (delete-region start end)
      (save-excursion
        (goto-char start)
        (insert (pp-to-string data))))))

(defun json-sexp-convert-region-to-json (start end)
  "Convert region from sexps to JSON."
  (interactive "r")
  (unless (= start end)
    (let ((data (car (read-from-string (buffer-substring start end)))))
      (delete-region start end)
      (save-excursion
        (goto-char start)
        (insert (let ((json-encoding-pretty-print t))
                  (json-encode data)))))))

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
  (json-sexp-convert-region-to-sexp (point-min) (point-max))
  (set-buffer-modified-p nil)
  (add-hook 'before-save-hook 'json-sexp-convert-buffer-to-json nil t)
  (add-hook 'after-save-hook 'json-sexp-after-save nil t))

(defun json-sexp-after-save ()
  (json-sexp-convert-buffer-to-sexp)
  (set-buffer-modified-p nil))

(defadvice json-read-object (around preserve-order)
  "Preserve the order of key/value pairs when converting to an
alist or plist."
  (setq ad-return-value
        (let ((object ad-do-it))
          (cond
           ((eq json-object-type 'alist)
            (nreverse object))
           ((eq json-object-type 'plist)
            (let ((reversed nil))
              (while object
                (setq reversed (cons (car object)
                                     (cons (cadr object) reversed)))
                (setq object (cddr object)))
              reversed))
           (t
            object)))))

(provide 'json-sexp-mode)
;;; json-sexp-mode.el ends here
