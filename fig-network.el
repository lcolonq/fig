;;; fig-network --- Pub/sub bus client -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'dash)
(require 's)

(defgroup fig nil
  "Pub/sub bus client."
  :group 'applications)

(defcustom fig/network-buffer " *fig-network*"
  "Name of buffer used to store intermediate network data."
  :type '(string)
  :group 'fig)

(defcustom fig/log-buffer "*fig-log*"
  "Name of buffer used to store the log."
  :type '(string)
  :group 'fig)

(defun fig/slurp (path)
  "Read PATH and return a string."
  (with-temp-buffer
    (insert-file-contents-literally path)
    (buffer-string)))

(defconst fig/host "shiro")
;; (defconst fig/host "localhost")
(defconst fig/port 32050)

(defvar fig//event-handlers nil)

(defun fig//write (text &optional face)
  "Write TEXT to the current buffer and apply FACE."
  (let ((text-final (if face (propertize text 'face face) text)))
    (insert text-final)))

(defun fig//write-line (line &optional face)
  "Write LINE and a newline to the current buffer and apply FACE."
  (fig//write (concat line "\n") face))

(defun fig//clean-string (s)
  "Remove special characters from S."
  (replace-regexp-in-string "[^[:print:]]" "" s))

(defun fig//write-log (line &optional face)
  "Write LINE to the log buffer and apply FACE."
  (with-current-buffer (get-buffer-create fig/log-buffer)
    (goto-char (point-max))
    (fig//write-line (fig//clean-string (format "%s" line)) face)
    (goto-char (point-max))))

(defun fig//handle-message (msg)
  "Handle the message MSG."
  (let* ((ev (car msg))
         (body (cdr msg))
         (handler (alist-get ev fig//event-handlers nil nil #'equal)))
    (if handler
        (funcall handler body)
      (fig//write-log (format "Unknown incoming event: %S" ev)))))

(defun fig//get-complete-line ()
  "Kill a line followed by a newline if it exists, and nil otherwise."
  (let ((l (thing-at-point 'line t)))
    (if (and l (s-contains? "\n" l))
        (progn
          (delete-region (line-beginning-position) (line-beginning-position 2))
          l)
      nil)))
(defun fig//handle-lines ()
  "Call `fig//handle-message' on every complete line of the current buffer."
  (let ((l (fig//get-complete-line)))
    (when (and l (not (s-blank? l)))
      (fig//handle-message (read (fig//clean-string l)))
      (fig//handle-lines))))
(defun fig//process-filter (proc data)
  "Process filter for pub/sub bus connection on PROC and DATA."
  (with-current-buffer (get-buffer-create fig/network-buffer)
    (when (not (marker-position (process-mark proc)))
      (set-marker (process-mark proc) (point-max)))
    (goto-char (process-mark proc))
    (insert data)
    (set-marker (process-mark proc) (point))
    (goto-char (point-min))
    (fig//handle-lines)))

(defun fig/disconnect ()
  "Disconnect from the pub/sub bus."
  (when (process-live-p (get-process "fig"))
    (delete-process "fig")))

(defun fig/connect ()
  "Connect to the pub/sub bus."
  (fig/disconnect)
  (make-network-process
   :name "fig"
   :buffer nil
   :host fig/host
   :service fig/port
   :filter #'fig//process-filter)
  (fig/sub-all))

(defun fig/sub (ev)
  "Subscribe to the event EV."
  (process-send-string
   "fig"
   (s-concat
    (format "%S" `(sub ,ev))
    "\n")))

(defun fig/pub (ev &optional d)
  "Publish the data D to the event EV."
  (process-send-string
   "fig"
   (s-concat
    (format "%S" `(pub ,ev ,@d))
    "\n")))

(defun fig/sub-all ()
  "Subscribe to all events in `fig//event-handlers'."
  (--each fig//event-handlers
    (fig//write-log (format "Subscribing to: %S" (car it)))
    (fig/sub (car it))))

(provide 'fig-network)
;;; fig-network.el ends here
