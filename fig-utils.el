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

(defun fig/list-to-pair (xs)
  "Turn the first two elements of XS into a pair."
  (cons (car xs) (cadr xs)))

(defun fig/tempfile (prefix str &optional ext)
  "Write STR to a temporary file with PREFIX and return the path.
Optionally append EXT to the path."
  (let ((path (s-concat (make-temp-file prefix) (or ext ""))))
    (with-temp-file path (insert str))
    path))

(provide 'fig-utils)
;;; fig-utils.el ends here
