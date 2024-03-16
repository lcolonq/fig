;;; fig-utils --- Geiser counter and factions -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'dash)
(require 's)
(require 'f)
(require 'ht)
(require 'cl-lib)

(defun fig/random-elt (xs)
  "Pick a random element of XS."
  (nth (random (length xs)) xs))

(defun fig//shuffle-seq (s)
  "Shuffle S."
  (if (seq-empty-p s)
      nil
    (let ((elt (seq-elt s (random (seq-length s)))))
      (cons elt (fig//shuffle-seq (remove elt s))))))

(defun fig/list-to-pair (xs)
  "Turn the first two elements of XS into a pair."
  (cons (car xs) (cadr xs)))

(defun fig/tempfile (prefix str &optional ext)
  "Write STR to a temporary file with PREFIX and return the path.
Optionally append EXT to the path."
  (let ((path (s-concat (make-temp-file prefix) (or ext ""))))
    (with-temp-file path (insert str))
    path))

(defun fig//decode-string (s)
  "Decode the base64 UTF-8 string S."
  (decode-coding-string (base64-decode-string s) 'utf-8))

(defun fig//encode-string (s)
  "Decode the base64 UTF-8 string S."
  (base64-encode-string (encode-coding-string s 'utf-8) t))

(defvar fig//fetch-last-response nil)
(defun fig//fetch (url &optional k)
  "Get URL, passing the returned HTML to K."
  (request
    url
    :type "GET"
    :parser (lambda () (libxml-parse-html-region (point-min) (point-max)))
    :success
    (cl-function
     (lambda (&key data &allow-other-keys)
       (setq fig//fetch-last-response data)
       (when k
         (funcall k data)))))
  t)

(provide 'fig-utils)
;;; fig-utils.el ends here
