;;; fig-geiser --- Geiser counter and factions -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'dash)
(require 's)
(require 'f)
(require 'ht)
(require 'lunar)

(defvar fig//geiser-alts (list "notgeiser" "cbtcaptain" "mahjongpilled" "infant_yeetah" "mindgoblindeeznutslmao"))

(defvar fig//geisers-seen (list))

(defun fig//check-chatter-geiser (user)
  "Check if USER is Geiser and increment counter if so."
  (when (-contains? fig//geiser-alts user)
    (add-to-list 'fig//geisers-seen user)))

(defvar fig//faction-exemptions
  (list
   "LCOLONQ"
   "MODCLONK"
   "fn_lumi"))

(defun fig//determine-initial-faction (user)
  "Determine the initial faction for USER."
  (unless (-contains? fig//faction-exemptions user)
    (let* ((factions '(nate lever tony)))
      (nth (random (length factions)) factions))))

(defun fig//assign-chatter-faction (user)
  "If USER doesn't have a faction, assign them a faction."
  (fig//update-db-default
   user
   :faction
   (lambda (f)
     (if (and f (symbolp f)) f
       (fig//determine-initial-faction user)))
   nil))

(defun fig//get-chatter-faction (user)
  "Get the faction for USER."
  (fig//load-db-entry user :faction))

(defun fig//set-chatter-faction (user f)
  "Get the faction for USER to F."
  (fig//update-db-default user :faction (lambda (_) f) nil))

(defun fig//tally-faction-boosts ()
  "Return the boost totals for each faction."
  (let* ((users (--map (fig//load-db it) (fig//all-db-users)))
         (nate (-sum (--map (or (alist-get :boost it) 0)
                            (--filter (eq (alist-get :faction it) 'nate) users))))
         (tony (-sum (--map (or (alist-get :boost it) 0)
                            (--filter (eq (alist-get :faction it) 'tony) users))))
         (lever (-sum (--map (or (alist-get :boost it) 0)
                             (--filter (eq (alist-get :faction it) 'lever) users)))))
    (list nate tony lever)))

(defun fig//update-chat-boost-tally ()
  "Update the boost tally above chat."
  (let ((tally (fig//tally-faction-boosts)))
    (setq
     fig//chat-header-line
     (format
      (s-concat
       " faction standings | "
       (propertize "nate" 'face '(:foreground "red"))
       ": %s"
       " | "
       (propertize "tony" 'face '(:foreground "green"))
       ": %s"
       " | "
       (propertize "lever" 'face '(:foreground "blue"))
       ": %s")
      (car tally) (cadr tally) (caddr tally)))))
(fig//update-chat-boost-tally)

(defun fig//geiser-counter ()
  "Return the number of Geisers seen this stream."
  (length fig//geisers-seen))

(provide 'fig-geiser)
;;; fig-geiser.el ends here
