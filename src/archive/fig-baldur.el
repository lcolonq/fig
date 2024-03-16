;;; fig-baldur --- Baldur's gate control -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'dash)
(require 's)
(require 'f)
(require 'ht)

(defun fig//baldur-cmd (cmd)
  "Send CMD to Baldur's Gate."
  (fig/pub '(bald event) (list cmd)))

(defun fig//baldur-click (x y)
  "Click X and Y in Baldur's Gate."
  (fig/pub '(bald event) (list (format "click %s %s" x y))))

(defun fig//baldur-key (key)
  "Press KEY in Baldur's Gate."
  (fig/pub '(bald event) (list (format "key %s" key))))

(provide 'fig-baldur)
;;; fig-baldur.el ends here
