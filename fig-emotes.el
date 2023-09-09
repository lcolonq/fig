;;; fig-emotes --- Emote caching and display -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'dash)
(require 's)
(require 'f)

(defcustom fig/emote-cache-dir "/home/llll/src/fig/emotes/"
  "The directory in which to store downloaded emote images."
  :type '(string)
  :group 'fig)

(defun fig//add-image-over (image msg start end)
  "Add IMAGE to MSG between START and END."
  (with-temp-buffer
    (insert msg)
    (add-text-properties
     start end
     `(display
       ,image
       rear-nonsticky t))
    (buffer-string)))

(defun fig//emote-path (emoteid)
  "Get the canonical path EMOTEID."
  (s-concat fig/emote-cache-dir emoteid))

(defun fig//download-emote (emoteid)
  "Ensure that EMOTEID exists in the cache."
  (let* ((path (fig//emote-path emoteid))
         (url (format "https://static-cdn.jtvnw.net/emoticons/v2/%s/default/dark/1.0" emoteid)))
    (unless (f-exists? path)
      (make-process
       :name "fig-download-emote"
       :buffer nil
       :command (list "curl" "-L" url "-o" path)
      ))))

(defvar fig/emotes nil)
(defun fig//save-emotes ()
  "Save the emotes database."
  (fig//save-db "__EMOTES__" fig/emotes))
(defun fig//load-emotes ()
  "Load the emotes database."
  (setf fig/emotes (fig//load-db "__EMOTES__")))
(defun fig//add-emote (enm eid)
  "Add emote ENM with EID."
  (add-to-list 'fig/emotes (cons (substring-no-properties enm) eid))
  (fig//save-emotes))
(defun fig//get-emote (enm)
  "Get the ID for ENM."
  (alist-get enm fig/emotes nil nil #'s-equals?))
(fig//load-emotes)

(defun fig//process-emote-range (er msg)
  "Given a string ER of form emoteid:start-end, add the emote MSG."
  (if (string-empty-p er)
      msg
    (when-let* ((er-split (s-split ":" er))
                (emoteid (car er-split))
                (range-split (s-split "-" (cadr er-split)))
                (start (string-to-number (car range-split)))
                (end (string-to-number (cadr range-split)))
                (emotemsg (substring msg start (+ end 1)))
                (path (fig//emote-path emoteid)))
      (fig//add-emote emotemsg emoteid)
      (fig//download-emote emoteid)
      (let ((img (create-image path)))
        (fig//add-image-over img msg (+ start 1) (+ end 2))
        ))))

(defun fig//process-emote-ranges (ers msg)
  "Apply all of ERS to MSG."
  (--reduce-from (fig//process-emote-range it acc) msg ers))

(defvar fig//emote-frame-counter 0)

(defun fig//advance-frame-in-chat-buffer ()
  "Advance all animated emotes in the (visible) chat buffer by 1 frame."
  (cl-incf fig//emote-frame-counter)
  (save-excursion
    (with-current-buffer (fig//get-twitch-chat-buffer)
      (goto-char (point-max))
      (forward-line -10)
      (goto-char (line-beginning-position))
      (while (not (eobp))
        (let ((plist (text-properties-at (point)))
              (next-change
               (or (next-property-change (point) (current-buffer))
                   (point-max))))
          (when-let* ((plist-true plist)
                      (disp (plist-get plist 'display))
                      (is-image (equal (car disp) 'image))
                      (image-props (cdr disp))
                      (image-type (plist-get image-props :type))
                      (is-gif (equal image-type 'gif))
                      (multi-frame (or (plist-get (cdr disp) :animate-multi-frame-data) (image-multi-frame-p disp)))
                      )
            (let ((frame (% fig//emote-frame-counter (car multi-frame))))
              (image-show-frame disp frame)))
          (goto-char next-change))))))

(defvar fig//emote-frame-timer nil)
(defun fig//run-emote-frame-timer ()
  "Run the emote frame timer."
  (when fig//emote-frame-timer
    (cancel-timer fig//emote-frame-timer))
  (fig//advance-frame-in-chat-buffer)
  (setq
   fig//emote-frame-timer
   (run-with-timer 0.03 nil #'fig//run-emote-frame-timer)))
(fig//run-emote-frame-timer)

(provide 'fig-emotes)
;;; fig-emotes.el ends here
