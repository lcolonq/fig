;;; fig-thanks --- Giving thanks -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'dash)
(require 's)
(require 'f)
(require 'ht)
(require 'cl-lib)

(defcustom fig/gl-buffer "*fig-gl*"
  "Name of buffer used to display software render."
  :type '(string)
  :group 'fig)

(define-derived-mode fig/gl-mode special-mode "GL"
  "Major mode for displaying software render."
  :group 'fig
  (setq-local cursor-type nil))

(defun fig//get-gl-buffer ()
  "Return the GL buffer."
  (unless (get-buffer fig/gl-buffer)
    (with-current-buffer (get-buffer-create fig/gl-buffer)
      (fig/gl-mode)))
  (get-buffer fig/gl-buffer))

(provide 'fig-thanks)
;;; fig-thanks.el ends here
