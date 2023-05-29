;;; fig-semi --- colonq.computer/semi -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 's)

(defun fig//get-semi-src-buffer (userid)
  "Get the buffer for colonq.computer/semi source for USERID."
  (get-buffer-create (s-concat "*fig-semi-src " userid "*")))

(defun fig//populate-semi-src-buffer (userid)
  "Populate the buffer colonq.computer/semi source for USERID."
  (let ((buffer (fig//get-semi-src-buffer userid)))
    (with-current-buffer buffer
      (janet-mode)
      (erase-buffer)
      (insert "# Loading...\n"))
    (make-process
     :name "fig-get-semi-src"
     :buffer buffer
     :stderr (get-buffer-create "*fig-semi-src-error*")
     :command (list "get_semi_src" userid)
     :sentinel
     (lambda (process _)
       (with-current-buffer (process-buffer process)
         (goto-char (point-min))
         (delete-line))))
    buffer))
(defun fig//semi-src (userid)
  "Switch to the colonq.computer/semi source buffer for USERID."
  (switch-to-buffer-other-window (fig//populate-semi-src-buffer userid))
  nil)

(provide 'fig-semi)
;;; fig-semi.el ends here
