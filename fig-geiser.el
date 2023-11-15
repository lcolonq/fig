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
  (unless (fig//get-chatter-faction user)
    (fig//update-db-default
     user
     :faction
     (lambda (f)
       (if (and f (symbolp f)) f
         (fig//determine-initial-faction user)))
     nil)))

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
       (propertize "lever" 'face '(:foreground "lightblue"))
       ": %s")
      (car tally) (cadr tally) (caddr tally)))))
(fig//update-chat-boost-tally)

(defun fig//geiser-counter ()
  "Return the number of Geisers seen this stream."
  (length fig//geisers-seen))

(defun fig//stock-price (s)
  "Get the current stock price for S."
  (string-to-number
   (s-replace "$" "" (s-trim (shell-command-to-string (format "stock %s" s))))))

(defun fig//change-theme (theme)
  "Change the current theme to THEME."
  (ef-themes-select theme)
  (ef-themes-with-colors
    (setenv "COLONQ_BGCOLOR" bg-main)
    (set-face-attribute 'vertical-border nil
                        :foreground bg-alt
                        :background bg-alt)
    (set-face-attribute 'fringe nil
                        :foreground bg-alt
                        :background bg-alt))
  (ef-themes-with-colors
    (set-face-attribute 'eshell-prompt nil
                        :foreground fg-main
                        :background bg-alt
                        :weight 'bold
                        :extend t)))

(defun fig//get-load ()
  "Get the current CPU load."
  (let ((res (shell-command-to-string "uptime")))
    (string-to-number (s-trim (car (s-split "," (cadr (s-split "load average:" res))))))))

(defun fig//get-heartrate ()
  "Get the streamer's heart rate."
  (* 100 (fig//get-load)))

(defcustom fig/heartrate-buffer "*fig-heartrate*"
  "Name of buffer used to ."
  :type '(string)
  :group 'fig)

(define-derived-mode fig/heartrate-mode special-mode "heartrate"
  "Major mode for displaying heartrate."
  :group 'fig
  (setq-local cursor-type nil))

(defun fig//get-heartrate-buffer ()
  "Return the heartrate buffer."
  (unless (get-buffer fig/heartrate-buffer)
    (with-current-buffer (get-buffer-create fig/heartrate-buffer)
      (fig/heartrate-mode)))
  (get-buffer fig/heartrate-buffer))

(defun fig//render-heartrate ()
  "Render the heartrate buffer."
  (with-current-buffer (fig//get-heartrate-buffer)
    (setq-local cursor-type nil)
    (let* ((inhibit-read-only t))
      (erase-buffer)
      (fig//write (format "%3d bpm" (fig//get-heartrate))))))

(defvar fig//heartrate-timer nil)
(defun fig//run-heartrate-timer ()
  "Run the heartrate timer."
  (when fig//heartrate-timer
    (cancel-timer fig//heartrate-timer))
  (fig//render-heartrate)
  (setq
   fig//heartrate-timer
   (run-with-timer 1 nil #'fig//run-heartrate-timer)))
(fig//run-heartrate-timer)

(provide 'fig-geiser)
;;; fig-geiser.el ends here
