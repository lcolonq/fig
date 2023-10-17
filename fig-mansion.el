;;; fig-mansion --- living in a mansion with the clonkheads -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'dash)
(require 's)
(require 'f)
(require 'ht)
(require 'cl-lib)

(defconst fig//mansion-preroom-path "/home/llll/src/mansion/preroom")
(defconst fig//mansion-intermediate-path "/home/llll/src/mansion/intermediate.gif")
(defconst fig//mansion-label-path "/home/llll/src/mansion/label.gif")

(defvar fig//mansion-current-room-occupant nil)
(defvar fig//mansion-current-room nil)

(defun fig//get-emote-color (emote)
  "Return whether EMOTE is red, green, or blue."
  (let* ((eid (fig//get-emote emote))
         (7id (fig//get-7tv-emote emote))
         (epath
          (cond
           (eid (fig//emote-path eid))
           (7id (fig//7tv-emote-path 7id))
           (t nil))))
    (when epath
      (s-trim
       (shell-command-to-string
        (s-concat
         "/home/llll/src/fengshui/target/release/fengshui "
         epath))))))

(defun fig//mansion-inspect-intermediate ()
  "Look at the current room being decorated."
  (browse-url (format "file://%s" fig//mansion-intermediate-path)))

(defun fig//mansion-inspect-room (roomnum)
  "Look at the room ROOMNUM."
  (browse-url (format "https://colonq.computer/room/%s.gif" roomnum)))

(defun fig//mansion-fetch-room (url k)
  "Retrieve URL from the web and place it in the appropriate zone.
Call K when complete."
  (make-process
   :name "fig-mansion-fetch-room"
   :buffer nil
   :command (list "curl" "-L" url "-o" fig//mansion-preroom-path)
   :sentinel
   (lambda (_ _)
     (funcall k))
   ))

(defun fig//mansion-prepare-room (k)
  "Prepare the current pre-room and call K when complete."
  (make-process
   :name "fig-mansion-prepare-room"
   :buffer nil
   :command (list "/home/llll/src/mansion/prepare_room.sh" fig//mansion-preroom-path)
   :sentinel
   (lambda (_ _)
     (funcall k))
   ))


(defun fig//mansion-write-label (text k)
  "Write TEXT to the label and call K when complete."
  (make-process
   :name "fig-mansion-write-label"
   :buffer nil
   :command (list "/home/llll/src/mansion/write_label.sh" text)
   :sentinel
   (lambda (_ _)
     (funcall k))
   ))

(defun fig//mansion-furnish-room (path x y k)
  "Add the image at PATH to X, Y and call K when complete."
  (make-process
   :name "fig-mansion-furnish-room"
   :buffer nil
   :command (list "/home/llll/src/mansion/add_furniture.sh" path (format "%s" x) (format "%s" y))
   :sentinel
   (lambda (_ _)
     (funcall k))
   ))

(defun fig//mansion-add-label (k)
  "Add the label to the current room and call K when complete."
  (make-process
   :name "fig-mansion-add-label"
   :buffer nil
   :command (list "/home/llll/src/mansion/add_label.sh")
   :sentinel
   (lambda (_ _)
     (funcall k))
   ))

(defun fig//mansion-place-emote-cell (emote cx cy k)
  "Place EMOTE at cell CX, CY and call K when complete."
  (let ((eid (fig//get-emote emote))
        (7id (fig//get-7tv-emote emote))
        (x (* cx 64))
        (y (* cy 64)))
    (fig//write-chat-event (format "Placing %s at (%s, %s)" emote cx cy))
    (push (cons (cons cx cy) emote) fig//mansion-current-room)
    (cond
     (eid (fig//mansion-furnish-room (fig//emote-path eid) x y k))
     (7id (fig//mansion-furnish-room (fig//7tv-emote-path 7id) x y k))
     (t (fig//write-log (s-concat "failed to find emote: " emote))))))

(defun fig//mansion-label-room (text k)
  "Add TEXT as a label to the current room.
Call K when done."
  (fig//mansion-write-label
   text
   (lambda ()
     (fig//mansion-add-label k))))

(defun fig//mansion-occupy-room (user)
  "Place USER in the current room.
Call K when done."
  (setf fig//mansion-current-room-occupant user)
  (let ((path (fig//twitch-user-avatar-path user)))
      (fig//mansion-label-room
       (s-concat "resident: " user)
       (lambda ()
         (fig//twitch-get-user-avatar
          user
          (lambda ()
            (when (f-exists? path)
              (fig//mansion-furnish-room
               path 96 96
               (lambda () nil)))))))))

(defun fig//mansion-rent-room (roomnum)
  "Add the image for ROOMNUM to the site.
Call K when complete."
  (fig//set-db-entry
   fig//mansion-current-room-occupant
   :room
   (list roomnum fig//mansion-current-room))
  (setf fig//mansion-current-room-occupant nil)
  (setf fig//mansion-current-room nil)
  (make-process
   :name "fig-mansion-rent-room"
   :buffer nil
   :command (list "/home/llll/src/mansion/rent_room.sh" (format "%s" roomnum))
   :sentinel
   (lambda (_ _)
     (fig//twitch-say (s-concat "Here are your keys: " (format "https://colonq.computer/room/%s.gif" roomnum)))
     (message "Finished renting room."))))

(defun fig//mansion-setup-room (url)
  "Fetch and prepare the room at URL."
  (fig//mansion-fetch-room
   url
   (lambda ()
     (fig//mansion-prepare-room
      (lambda ()
        (message "Ready to furnish room."))))))

(provide 'fig-mansion)
;;; fig-mansion.el ends here
