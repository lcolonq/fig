;;; fig-geiser --- Geiser counter -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'dash)
(require 's)
(require 'f)
(require 'ht)

(defvar fig//geiser-alts (list "notgeiser" "cbtcaptain" "mahjongpilled" "infant_yeetah" "mindgoblindeeznutslmao"))

(defvar fig//geisers-seen (list))

(defun fig//check-chatter-geiser (user)
  "Check if USER is Geiser and increment counter if so."
  (when (-contains? fig//geiser-alts user)
    (add-to-list 'fig//geisers-seen user)))

(defun fig//geiser-counter ()
  "Return the number of Geisers seen this stream."
  (length fig//geisers-seen))

(provide 'fig-geiser)
;;; fig-geiser.el ends here
