;;; fig-piano --- Control Jake's piano -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'dash)
(require 's)
(require 'f)
(require 'ht)

(defun fig//play-piano-note (n)
  "Play note N."
  (fig/pub '(monitor bullfrog broadcast) (list (format "%s" n))))

(defvar fig//note-number-names
  '((60 . "C")
    (62 . "D")
    (64 . "E")
    (65 . "F")
    (67 . "G")
    (69 . "A")
    (71 . "B")
    ))

(defun fig//get-note-name (note)
  "Get the string name for NOTE."
  (or (alist-get note fig//note-number-names) ""))

(defun fig//get-chatter-note (username)
  "Get note for USERNAME."
  (let ((old (alist-get :note (fig//load-db username))))
    (if old old
      (let ((new (nth (random 7) (list 60 62 64 65 67 69 71))))
        (fig//update-db-number username :note (lambda (_) new))
        new))))

(provide 'fig-piano)
;;; fig-piano.el ends here
