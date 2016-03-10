;;; jsxp.el --- JSON parser/generator fine-tuned for json-sexp-mode

;; Copyright (C) 2006-2016 Free Software Foundation, Inc.

;; Author: Taylan Ulrich Bayırlı/Kammer <taylanbayirli@gmail.com>
;; Version: 1.0
;; Keywords: convenience

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

;; This is originally based on the upstream json.el.

;; The user-serviceable entry points for the parser are the functions
;; `jsxp-read' and `jsxp-read-from-string'. The encoder has a single entry
;; point, `jsxp-encode'.

;; We use quasi-alists to represent objects:
;; 
;; { "k1": v1, "k2": v2, ... } -> ((k1 v1) (k2 v2) ...)
;;
;; We use Elisp vectors for JSON arrays, Elisp strings and numbers for JSON
;; strings and numbers, and the symbols `true', `false', and `null' to represent
;; the corresponding JSON values.

;;; Code:

(defvar jsxp--encoding-current-indentation "\n"
  "Internally used to keep track of the current indentation level of encoding.")

;;; Utilities

(defsubst jsxp-join (strings separator)
  "Join STRINGS with SEPARATOR."
  (mapconcat 'identity strings separator))

(defmacro jsxp--with-indentation (body)
  `(let ((jsxp--encoding-current-indentation
          (concat jsxp--encoding-current-indentation
                  "  ")))
     ,body))

;; Reader utilities

(defsubst jsxp-advance (&optional n)
  "Skip past the following N characters."
  (forward-char n))

(defsubst jsxp-peek ()
  "Return the character at point."
  (let ((char (char-after (point))))
    (or char :jsxp-eof)))

(defsubst jsxp-pop ()
  "Advance past the character at point, returning it."
  (let ((char (jsxp-peek)))
    (if (eq char :jsxp-eof)
        (signal 'end-of-file nil)
      (jsxp-advance)
      char)))

(defsubst jsxp-skip-whitespace ()
  "Skip past the whitespace at point."
  (skip-chars-forward "\t\r\n\f\b "))



;; Error conditions

(define-error 'jsxp-error "Unknown JSON error")
(define-error 'jsxp-readtable-error "JSON readtable error" 'jsxp-error)
(define-error 'jsxp-unknown-keyword "Unrecognized keyword" 'jsxp-error)
(define-error 'jsxp-number-format "Invalid number format" 'jsxp-error)
(define-error 'jsxp-string-escape "Bad Unicode escape" 'jsxp-error)
(define-error 'jsxp-string-format "Bad string format" 'jsxp-error)
(define-error 'jsxp-key-format "Bad JSON object key" 'jsxp-error)
(define-error 'jsxp-object-format "Bad JSON object" 'jsxp-error)



;;; Keywords

;; Keyword parsing

(defun jsxp-read-keyword (keyword)
  "Read a JSON keyword at point."
  (mapc (lambda (char)
          (unless (char-equal char (jsxp-peek))
            (signal 'jsxp-unknown-keyword
                    (list (save-excursion
                            (backward-word 1)
                            (thing-at-point 'word)))))
          (jsxp-advance))
        keyword)
  (unless (looking-at "\\(\\s-\\|[],}]\\|$\\)")
    (signal 'jsxp-unknown-keyword
            (list (save-excursion
                    (backward-word 1)
                    (thing-at-point 'word)))))
  (cond ((string-equal keyword "true") 'true)
        ((string-equal keyword "false") 'false)
        ((string-equal keyword "null") 'null)
        (t (signal 'jsxp-unknown-keyword (list keyword)))))

;; Keyword encoding

(defsubst jsxp-encode-keyword (keyword)
  "Encode KEYWORD as a JSON value."
  (cond ((eq keyword 'true)  "true")
        ((eq keyword 'false) "false")
        ((eq keyword 'null)  "null")
        (t (signal 'jsxp-unknown-keyword (list keyword)))))

;;; Numbers

;; Number parsing

(defun jsxp-read-number (&optional sign)
 "Read the JSON number following point.
The optional SIGN argument is for internal use.

N.B.: Only numbers which can fit in Emacs Lisp's native number
representation will be parsed correctly."
 ;; If SIGN is non-nil, the number is explicitly signed.
 (let ((number-regexp
        "\\([0-9]+\\)?\\(\\.[0-9]+\\)?\\([Ee][+-]?[0-9]+\\)?"))
   (cond ((and (null sign) (char-equal (jsxp-peek) ?-))
          (jsxp-advance)
          (- (jsxp-read-number t)))
         ((and (null sign) (char-equal (jsxp-peek) ?+))
          (jsxp-advance)
          (jsxp-read-number t))
         ((and (looking-at number-regexp)
               (or (match-beginning 1)
                   (match-beginning 2)))
          (goto-char (match-end 0))
          (string-to-number (match-string 0)))
         (t (signal 'jsxp-number-format (list (point)))))))

;; Number encoding

(defsubst jsxp-encode-number (number)
  "Return a JSON representation of NUMBER."
  (format "%s" number))

;;; Strings

(defvar jsxp-special-chars
  '((?\" . ?\")
    (?\\ . ?\\)
    (?b . ?\b)
    (?f . ?\f)
    (?n . ?\n)
    (?r . ?\r)
    (?t . ?\t))
  "Characters which are escaped in JSON, with their elisp counterparts.")

;; String parsing

(defsubst jsxp-read-escaped-char ()
  "Read the JSON string escaped character at point."
  ;; Skip over the '\'
  (jsxp-advance)
  (let* ((char (jsxp-pop))
         (special (assq char jsxp-special-chars)))
    (cond
     (special (cdr special))
     ((not (eq char ?u)) char)
     ((looking-at "[0-9A-Fa-f][0-9A-Fa-f][0-9A-Fa-f][0-9A-Fa-f]")
      (let ((hex (match-string 0)))
        (jsxp-advance 4)
        (string-to-number hex 16)))
     (t
      (signal 'jsxp-string-escape (list (point)))))))

(defun jsxp-read-string ()
  "Read the JSON string at point."
  (unless (char-equal (jsxp-peek) ?\")
    (signal 'jsxp-string-format (list "doesn't start with `\"'!")))
  ;; Skip over the '"'
  (jsxp-advance)
  (let ((characters '())
        (char (jsxp-peek)))
    (while (not (char-equal char ?\"))
      (push (if (char-equal char ?\\)
                (jsxp-read-escaped-char)
              (jsxp-pop))
            characters)
      (setq char (jsxp-peek)))
    ;; Skip over the '"'
    (jsxp-advance)
    (if characters
        (apply 'string (nreverse characters))
      "")))

;; String encoding

(defun jsxp-encode-string (string)
  "Return a JSON representation of STRING."
  ;; Reimplement the meat of `replace-regexp-in-string', for
  ;; performance (bug#20154).
  (let ((l (length string))
        (start 0)
        res mb)
    ;; Only escape quotation mark, backslash and the control
    ;; characters U+0000 to U+001F (RFC 4627, ECMA-404).
    (while (setq mb (string-match "[\"\\[:cntrl:]]" string start))
      (let* ((c (aref string mb))
             (special (rassq c jsxp-special-chars)))
        (push (substring string start mb) res)
        (push (if special
                  ;; Special JSON character (\n, \r, etc.).
                  (string ?\\ (car special))
                ;; Fallback: UCS code point in \uNNNN form.
                (format "\\u%04x" c))
              res)
        (setq start (1+ mb))))
    (push (substring string start l) res)
    (push "\"" res)
    (apply #'concat "\"" (nreverse res))))

;;; JSON Objects

(defsubst jsxp-new-object ()
  "Create a new Elisp object corresponding to a JSON object.
Please see the documentation of `jsxp-object-type'."
  (list))

(defsubst jsxp-add-to-object (object key value)
  "Add a new KEY -> VALUE association to OBJECT.
Returns the updated object, which you should save, e.g.:
    (setq obj (jsxp-add-to-object obj \"foo\" \"bar\"))"
  (cons (list (make-symbol key) value) object))

;; JSON object parsing

(defun jsxp-read-object ()
  "Read the JSON object at point."
  ;; Skip over the "{"
  (jsxp-advance)
  (jsxp-skip-whitespace)
  ;; read key/value pairs until "}"
  (let ((elements (jsxp-new-object))
        key value)
    (while (not (char-equal (jsxp-peek) ?}))
      (jsxp-skip-whitespace)
      (setq key (jsxp-read-string))
      (jsxp-skip-whitespace)
      (if (char-equal (jsxp-peek) ?:)
          (jsxp-advance)
        (signal 'jsxp-object-format (list ":" (jsxp-peek))))
      (setq value (jsxp-read))
      (setq elements (jsxp-add-to-object elements key value))
      (jsxp-skip-whitespace)
      (unless (char-equal (jsxp-peek) ?})
        (if (char-equal (jsxp-peek) ?,)
            (jsxp-advance)
          (signal 'jsxp-object-format (list "," (jsxp-peek))))))
    ;; Skip over the "}"
    (jsxp-advance)
    ;; Preserve entry order
    (nreverse elements)))

;; Table encoding

(defun jsxp-encode-table (table)
  "Return a JSON representation of TABLE."
  (if (null table)
      "{}"
    (format "{%s%s}"
            (jsxp-join
             (jsxp--with-indentation
              (mapcar (lambda (cons)
                        (format "%s%s: %s"
                                jsxp--encoding-current-indentation
                                (symbol-name (car cons))
                                (jsxp-encode (cadr cons))))
                      table))
             ",")
            jsxp--encoding-current-indentation)))

;;; Arrays

;; Array parsing

(defun jsxp-read-array ()
  "Read the JSON array at point."
  ;; Skip over the "["
  (jsxp-advance)
  (jsxp-skip-whitespace)
  ;; read values until "]"
  (let (elements)
    (while (not (char-equal (jsxp-peek) ?\]))
      (push (jsxp-read) elements)
      (jsxp-skip-whitespace)
      (unless (char-equal (jsxp-peek) ?\])
        (if (char-equal (jsxp-peek) ?,)
            (jsxp-advance)
          (signal 'jsxp-error (list 'bleah)))))
    ;; Skip over the "]"
    (jsxp-advance)
    (apply #'vector (nreverse elements))))

;; Array encoding

(defun jsxp-encode-array (array)
  "Return a JSON representation of ARRAY."
  (if (= (length array) 0)
      "[]"
    (concat
     (jsxp--with-indentation
      (concat (format "[%s" jsxp--encoding-current-indentation)
              (jsxp-join (mapcar 'jsxp-encode array)
                         (format "%s%s"
                                 ","
                                 jsxp--encoding-current-indentation))))
     (format "%s]" jsxp--encoding-current-indentation))))



;;; JSON reader.

(defvar jsxp-readtable
  (let ((table
         '((?t jsxp-read-keyword "true")
           (?f jsxp-read-keyword "false")
           (?n jsxp-read-keyword "null")
           (?{ jsxp-read-object)
           (?\[ jsxp-read-array)
           (?\" jsxp-read-string))))
    (mapc (lambda (char)
            (push (list char 'jsxp-read-number) table))
          '(?- ?+ ?. ?0 ?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8 ?9))
    table)
  "Readtable for JSON reader.")

(defun jsxp-read ()
  "Parse and return the JSON object following point.
Advances point just past JSON object."
  (jsxp-skip-whitespace)
  (let ((char (jsxp-peek)))
    (if (not (eq char :jsxp-eof))
        (let ((record (cdr (assq char jsxp-readtable))))
          (if (functionp (car record))
              (apply (car record) (cdr record))
            (signal 'jsxp-readtable-error record)))
      (signal 'end-of-file nil))))

;; Syntactic sugar for the reader

(defun jsxp-read-from-string (string)
  "Read the JSON object contained in STRING and return it."
  (with-temp-buffer
    (insert string)
    (goto-char (point-min))
    (jsxp-read)))

(defun jsxp-read-file (file)
  "Read the first JSON object contained in FILE and return it."
  (with-temp-buffer
    (insert-file-contents file)
    (goto-char (point-min))
    (jsxp-read)))



;;; JSON encoder

(defun jsxp-encode (object)
  "Return a JSON representation of OBJECT as a string."
  (cond ((memq object '(true false null)) (jsxp-encode-keyword object))
        ;; Check for list before symbol because `nil' is a symbol too.
        ((listp object)    (jsxp-encode-table object))
        ((vectorp object)  (jsxp-encode-array object))
        ((stringp object)  (jsxp-encode-string object))
        ((symbolp object)  (jsxp-encode-string (symbol-name object)))
        ((numberp object)  (jsxp-encode-number object))
        (t                 (signal 'jsxp-error (list object)))))

;; Pretty printing

(defun jsxp-pretty-print-buffer ()
  "Pretty-print current buffer."
  (interactive)
  (jsxp-pretty-print (point-min) (point-max)))

(defun jsxp-pretty-print (begin end)
  "Pretty-print selected region."
  (interactive "r")
  (atomic-change-group
    (let ((txt (delete-and-extract-region begin end)))
      (insert (jsxp-encode (jsxp-read-from-string txt))))))

(provide 'jsxp)

;;; jsxp.el ends here
