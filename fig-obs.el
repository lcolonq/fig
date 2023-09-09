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
(defvar fig//obs-intj-timer nil "Time to display INTJ stare.")
(defvar fig//obs-forsen-timer nil "Time to display Forsen.")
(defvar fig//obs-crit-timer nil "Time to display critical hit.")

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

(defun fig/intj-stare ()
  "Enable the INTJ Stare overlay for some time."
  (interactive)
  (unless fig//obs-intj-timer
    (fig//toggle-intj-stare))
  (setf fig//obs-intj-timer 17))

(defun fig/forsen ()
  "Enable Forsen mode for some time."
  (interactive)
  (unless fig//obs-forsen-timer
    (fig//model-toggle "forsen"))
  (setf fig//obs-forsen-timer 60))

(defun fig/critical-hit ()
  "Enable the Critical Hit overlay for some time."
  (interactive)
  (unless fig//obs-crit-timer
    (fig//toggle-critical-hit)
    (setf fig//obs-crit-timer 3)))

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

(defun fig//toggle-intj-stare ()
  "Toggle the INTJ Stare overlay."
  (fig/pub '(monitor obs toggle) (list "INTJ" "INTJ Image")))

(defun fig//toggle-critical-hit ()
  "Toggle the Critical Hit overlay."
  (fig/pub '(monitor obs toggle) (list "Critical Hit Wrapper" "Critical Hit")))

(defun fig//toggle-vhs ()
  "Toggle the VHS overlay."
  (fig/pub '(monitor obs toggle) (list "VHS" "VHS Group")))

(defun fig//toggle-saiyan ()
  "Toggle the Super Saiyan overlay."
  (fig/pub '(monitor obs toggle) (list "Saiyan" "Saiyan Video")))

(defun fig//toggle-persona4 ()
  "Toggle the Persona 4 dialogue box."
  (fig/pub '(monitor obs toggle) (list "Persona 4" "Persona 4 Background")))

(defun fig//toggle-explosion ()
  "Toggle the explosion effect."
  (fig/pub '(monitor obs toggle) (list "Explosion" "Explosion Video")))

(defun fig//set-clickbait-text (msg)
  "Change the clickbait text to MSG."
  (fig/pub '(monitor obs set-text) (list "Red Arrow Text" (fig//encode-string (s-trim msg)))))

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
  (when fig//obs-intj-timer
    (cl-decf fig//obs-intj-timer)
    (when (<= fig//obs-intj-timer 0)
      (setf fig//obs-intj-timer nil)
      (fig//toggle-intj-stare)))
  (when fig//obs-forsen-timer
    (cl-decf fig//obs-forsen-timer)
    (when (<= fig//obs-forsen-timer 0)
      (setf fig//obs-forsen-timer nil)
      (fig//model-toggle "forsen")))
  (when fig//obs-crit-timer
    (cl-decf fig//obs-crit-timer)
    (when (<= fig//obs-crit-timer 0)
      (setf fig//obs-crit-timer nil)
      (fig//toggle-critical-hit)))
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
