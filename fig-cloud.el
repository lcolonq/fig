;;; fig-cloud --- Clonk on the cloud -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'dash)
(require 's)
(require 'f)
(require 'ht)
(require 'cl-lib)

(defun fig//cloud-update ()
  "Send the current buffer and trigger refresh."
  (interactive)
  (fig//cloud-upload-buffer)
  (fig/pub '(monitor bullfrog broadcast) (list "hi")))

(add-hook 'post-command-hook #'fig//cloud-update)

(defun fig//cloud-collect-metadata (buf)
  "Collect relevant metadata from BUF."
  (save-excursion
    (with-current-buffer buf
      (goto-char (point-min))
      (let ((last (point-min))
            (ret nil)
            (cur nil))
        (setq cur (next-single-property-change last 'face))
        (while (and cur (not (= (point-max) cur)) (not (eobp)))
          (when-let* ((face (get-text-property last 'face))
                      (foreground (face-attribute face :foreground))
                      ((not (equal foreground 'unspecified))))
            (push (cons (format "%s" (- last 1)) `[,(- cur 1) ,foreground]) ret))
          (setq last cur)
          (setq cur (next-single-property-change last 'face)))
        ret))))

(defun fig//cloud-summarize-buffer (buf)
  "Summarize the contents of BUF."
  (let* ((str (with-current-buffer buf (buffer-substring (point-min) (point-max)))))
    (json-serialize
     (ht-from-alist
      `(("text" . ,str)
        ("meta" . ,(ht-from-alist (fig//cloud-collect-metadata buf)))
        )))))

(defun fig//cloud-upload-buffer (&optional buf)
  "Upload BUF (or the current buffer) to Bullfrog."
  (interactive)
  (let* ((b (or buf (current-buffer)))
         (str (fig//cloud-summarize-buffer b)))
    (fig//bullfrog-set "buffer" str)))

(provide 'fig-cloud)
;;; fig-cloud.el ends here
