;;; fig-obs --- Interact with OBS -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'dash)
(require 's)
(require 'f)
(require 'ht)
(require 'cl-lib)

(defvar fig//obs-modclonk-timer nil "Time since last MODCLONK message.")
(defvar fig//obs-live-reaction-timer nil "Time to display Live LCOLONQ Reaction.")
(defvar fig//obs-thug-life-timer nil "Time to display Thug Life.")
(defvar fig//obs-clickbait-timer nil "Time to display clickbait.")

(defun fig//obs-log-modclonk-message ()
  "Record that MODCLONK sent a message."
  (unless fig//obs-modclonk-timer
    (fig//toggle-modclonk))
  (setf fig//obs-modclonk-timer 11))

(defun fig/live-reaction ()
  "Enable the Live LCOLONQ Reaction for some time."
  (interactive)
  (unless fig//obs-live-reaction-timer
    (fig//toggle-live-reaction))
  (setf fig//obs-live-reaction-timer 17))

(defun fig/thug-life ()
  "Enable the Thug Life overlay for some time."
  (interactive)
  (unless fig//obs-thug-life-timer
    (fig//toggle-thug-life))
  (setf fig//obs-thug-life-timer 17))

(defun fig/clickbait (msg)
  "Enable the clickbait for some time.
MSG is displayed above the red arrow."
  (interactive)
  (fig//set-clickbait-text msg)
  (unless fig//obs-clickbait-timer
    (fig//toggle-clickbait msg))
  (setf fig//obs-clickbait-timer 31))

(defun fig//toggle-live-reaction ()
  "Toggle the Live LCOLONQ Reaction panel."
  (fig/pub '(monitor obs toggle) (list "Live LCOLONQ Reaction" "Live Reaction")))

(defun fig//toggle-modclonk ()
  "Toggle the MODCLONK panel."
  (fig/pub '(monitor obs toggle) (list "MODCLONK" "MODCLONK Chibi")))

(defun fig//toggle-thug-life ()
  "Toggle the Thug Life overlay."
  (fig/pub '(monitor obs toggle) (list "Thug Life" "Thug Life Video")))

(defun fig//toggle-vhs ()
  "Toggle the VHS overlay."
  (fig/pub '(monitor obs toggle) (list "VHS" "VHS Video")))

(defun fig//set-clickbait-text (msg)
  "Change the clickbait text to MSG."
  (fig/pub '(monitor obs set-text) (list "Red Arrow Text" (s-trim msg))))

(defun fig//toggle-clickbait (&optional msg)
  "Toggle the clickbait arrow.
Optionally, change text to MSG."
  (when msg
    (fig//set-clickbait-text msg))
  (fig/pub '(monitor obs toggle) (list "Red Arrow" "Red Arrow Group")))

(defun fig//handle-obs ()
  "Run OBS actions."
  (when fig//obs-modclonk-timer
    (cl-decf fig//obs-modclonk-timer)
    (when (<= fig//obs-modclonk-timer 0)
      (setf fig//obs-modclonk-timer nil)
      (fig//toggle-modclonk)))
  (when fig//obs-live-reaction-timer
    (cl-decf fig//obs-live-reaction-timer)
    (when (<= fig//obs-live-reaction-timer 0)
      (setf fig//obs-live-reaction-timer nil)
      (fig//toggle-live-reaction)))
  (when fig//obs-thug-life-timer
    (cl-decf fig//obs-thug-life-timer)
    (when (<= fig//obs-thug-life-timer 0)
      (setf fig//obs-thug-life-timer nil)
      (fig//toggle-thug-life)))
  (when fig//obs-clickbait-timer
    (cl-decf fig//obs-clickbait-timer)
    (when (<= fig//obs-clickbait-timer 0)
      (setf fig//obs-clickbait-timer nil)
      (fig//toggle-clickbait))))

(defvar fig//obs-timer nil)
(defun fig//run-obs-timer ()
  "Run the obs timer."
  (when fig//obs-timer
    (cancel-timer fig//obs-timer))
  (fig//handle-obs)
  (setq
   fig//obs-timer
   (run-with-timer 1 nil #'fig//run-obs-timer)))
(fig//run-obs-timer)

(provide 'fig-obs)
;;; fig-obs.el ends here
