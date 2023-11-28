;;; fig-friend --- "friend", an Emacs buddy -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'dash)
(require 's)
(require 'f)
(require 'ht)
(require 'cl-lib)

(defcustom fig/friend-buffer "*fig-friend*"
  "Name of buffer used to display \"friend\"."
  :type '(string)
  :group 'fig)

(define-derived-mode fig/friend-mode special-mode "\"friend\"'s lair"
  "Major mode for displaying \"friend\"'s lair."
  :group 'fig
  (message "hi i'm \"friend\"")
  (setq-local cursor-type nil))

(defun fig//get-friend-buffer ()
  "Return the \"friend\" buffer."
  (unless (get-buffer fig/friend-buffer)
    (with-current-buffer (get-buffer-create fig/friend-buffer)
      (fig/friend-mode)))
  (get-buffer fig/friend-buffer))

(defconst fig//friend-tastes
  (let ((expensive (> (fig//stock-price "AMZN") 125))
        (moon (car (lunar-phase-for-date (calendar-current-date)))))
    (s-concat
     (cond
      ((-contains? '("New" "Waxing Crescent") moon) " You prefer warm foods like soups.")
      ((-contains? '("First Quarter" "Waxing Gibbous") moon) " You prefer to eat leafy greens and fruits.")
      ((-contains? '("Full" "Waning Gibbous") moon) " You prefer to eat barbeque and grilled meats.")
      ((-contains? '("Last Quarter" "Waning Crescent") moon) " You prefer to eat corn beans and squash.")
      (t "")
      )
     (if expensive " You have expensive taste in food and dislike any food that can be obtained cheaply." ""))))

;; states:
;; default
;; jumping
;; eating, eating0, eating1, eating2
;; chatting, chatting0
(defvar fig//friend-state 'default)
(defvar fig//friend-emotion "neutral")
(defvar fig//friend-message-cache nil)
(defvar fig//friend-state-timer 0)

(defvar fig//friend-animation 1)
(defvar fig//friend-speech "")
(defvar fig//friend-speech-timer 0)

(defun fig//friend-personality (msg k)
  "Given MSG, pass a string with more personality to K."
  (let ((call (s-concat fig//friend-emotion " | " msg)))
    (fig/ask
     call
     (lambda (new)
       (let ((sp (s-split "|" (s-trim new))))
         (if (= 2 (length sp))
             (progn
               (when (stringp (car sp))
                 (setf fig//friend-emotion (s-trim (car sp))))
               (when (stringp (cadr sp))
                 (let ((resp (s-trim (cadr sp))))
                   (push (cons call resp) fig//friend-message-cache)
                   (funcall k resp))))
           (let ((resp (s-trim new)))
             (push (cons call resp) fig//friend-message-cache)
             (funcall k resp)))))
     (s-concat
      "You are the personality of a desktop buddy named \"friend\". \"friend\" is irreverant but kind, and only speaks in lowercase. You are kind of dumb in a cute way and silly like a virtual pet. You live in the corner of LCOLONQ's stream and provide commentary on events. Given an emotional state and a description of an event that happened to you, please respond with a new emotional state and a short message in response considering your emotional state. The message should only be one clause."
      fig//friend-tastes
      )
     (cons "neutral | Mimeyu fed you an apple." (reverse (-take 5 (-map #'car fig//friend-message-cache))))
     (cons "happy | yum apple so good" (reverse (-take 5 (-map #'cdr fig//friend-message-cache))))
     )))

(defun fig//enemy-personality (msg k)
  "Given MSG, pass a string with more personality (enemy mode) to K."
  (fig/ask
   (s-concat fig//friend-emotion " | " msg)
   (lambda (new)
     (let ((sp (s-split "|" (s-trim new))))
       (when (= 2 (length sp))
         (when (stringp (car sp))
           (setf fig//friend-emotion (s-trim (car sp))))
         (when (stringp (cadr sp))
           (funcall k (s-trim (cadr sp)))))))
   (s-concat
    "You are the personality of a desktop buddy named \"enemy\". \"enemy\" is irreverant and rude. You are very intelligent in a cute way and mean like a snake. You live in the corner of LCOLONQ's stream and provide commentary on events. Given an emotional state and a description of an event that happened to you, please respond with a new emotional state and a short message in response considering your emotional state. The message should only be one clause."
    fig//friend-tastes
    )
   "neutral | notgeiser fed you bone hurting juice."
   "disdainful | I really dislike you strongly, notgeiser."
   ))

(defun fig//friend-set-state (st &optional time)
  "Set \"friend\"'s state to ST for TIME seconds."
  (setf fig//friend-state st)
  (setf fig//friend-state-timer (or time 5)))

(defun fig//friend-set-speech (msg &optional time)
  "Have \"friend\" say MSG for TIME."
  (fig//write-chat-event (s-concat "Friend says: " msg))
  (setf fig//friend-speech msg)
  (setf fig//friend-speech-timer (or time 5)))

(defun fig//friend-say (msg)
  "Have \"friend\" say MSG."
  (fig//friend-set-speech msg 5)
  (fig//friend-set-state 'chatting 5))

(defun fig//friend-feed (user food)
  "Call when USER fed FOOD to \"friend\"."
  (if (-contains? fig//geiser-alts user)
      (fig//enemy-personality
       (format "You dislike %s and they are your enemy. %s fed you %s" user user food)
       (lambda (msg)
         (fig//friend-set-speech msg 6)
         (fig//friend-set-state 'eating 6)))
    (fig//friend-personality
     (format "%s fed you %s" user food)
     (lambda (msg)
       (fig//friend-set-speech msg 6)
       (fig//friend-set-state 'eating 6)))))

(defun fig//friend-respond (ev)
  "Call when an event EV happens to \"friend\"."
  (fig//friend-personality
   ev
   (lambda (msg)
     (fig//friend-set-speech msg 10)
     (fig//friend-set-state 'chatting 10))))

(defun fig//friend-chat (user msg)
  "Call when USER sends MSG to \"friend\"."
  (if (-contains? fig//geiser-alts user)
      (fig//enemy-personality
       (format "You dislike %s and they are your enemy. %s says: %s" user user msg)
       (lambda (msg)
         (fig//friend-set-speech msg 10)
         (fig//friend-set-state 'chatting 10)))
    (fig//friend-respond (format "%s says: %s" user msg))))

(defun fig//callout-flycheck-error ()
  "Call to respond to a random Flycheck error in the current buffer."
  (when-let* ((errs (--filter (eq (flycheck-error-level it) 'error) flycheck-current-errors))
         (err (nth (random (length errs)) errs)))
    (fig//friend-respond
     (s-concat
      "LCOLONQ made an error while programming: "
      (flycheck-error-message err)))))

(defun fig//callout-blackjack ()
  "Call to respond to the current blackjack game state."
  (fig//friend-respond
   (format
    "We're playing blackjack, and the current hand value is %s."
    (fig//bj-hand-value fig//bj-current-hand))))

(defun fig//callout-holiday ()
  "Call to respond to the current holiday."
  (fig//friend-respond
   (format
    "It's Thanksgiving today! Say something on the topic of Thanksgiving please! %s"
    (if (= 0 (random 2))
        ""
      "LCOLONQ and friends are celebrating \"Forthsgiving\", where they humorously mix the topic of Thanksgiving with the Forth programming language."))))

(defun fig//callout-hexamedia ()
  "Call to respond to a random recent chatter's Hexamedia card collection."
  (let* ((users (-filter #'cdr (--map (cons (car it) (fig//load-db-entry (car it) :hexamedia-cards)) (-take 10 fig//incoming-chat-history))))
         (user (and users (nth (random (length users)) users)))
         (cards (cdr user))
         (coll (and cards (nth (random (length cards)) cards))))
    (when coll
      (fig//friend-respond
       (format
        "%s has collected %s out of 20 cards in the %s collection. Please mention the collection name and the person collecting."
        (car user)
        (cdr coll)
        (car coll))))))

(defun fig//callout-uwoomfie ()
  "Call to respond to a random recent chatter's Uwoomfie status."
  (let* ((users
          (-filter
           #'cdr
           (--map
            (cons (car it) (fig//get-uwoomfie-status (car it)))
            (-take 10 fig//incoming-chat-history))))
         (user (and users (nth (random (length users)) users)))
         (respond (if (fig//check-chatter-geiser 
         )
    (print user)
    (cl-case (cdr user)
      (cool (fig//friend-respond (format "According to uwu_to_owo, %s is a very cool person. Make sure to mention their username." (car user))))
      (honored (fig//friend-respond (format "According to uwu_to_owo, %s is an honorary viewer. Make sure to mention their username." (car user))))
      (t nil))))
         

(defun fig//callout-gcp ()
  "Call to respond to the current GCP dot."
  (fig//gcp-dot
   (lambda (d)
     (fig//friend-respond
      (format
       "The Global Consciousness Project indicator is currently as follows: %s"
       (fig//gcp-describe d))))))

(defun fig//get-friend-offset ()
  "Return the number of newlines to print before \"friend\"."
  (if (-contains? '(jumping) fig//friend-state)
      fig//friend-animation
    1))

(defun fig//get-friend-face ()
  "Return the eyes and mouth for \"friend\" as a list of strings."
  (cl-case fig//friend-state
    (jumping (list "^" "^" "ww"))

    (eating (list "v" "v" "<>"))
    (eating0 (list "v" "v" "<>"))
    (eating1 (list "-" "-" "mw"))
    (eating2 (list "-" "-" "wm"))

    (chatting (list ">" ">" "oo"))
    (chatting0 (list ">" ">" "~~"))

    (t (list "-" "-" "ww"))))

(defun fig//get-friend-bubble ()
  "Return the text bubble for \"friend\"."
  (if (> fig//friend-speech-timer 0)
      fig//friend-speech
    nil))

(defun fig//friend-random-event ()
  "Activate a random \"friend\" event."
  (cl-case (random 30)
    (0 (fig//callout-flycheck-error))
    (1 (fig//callout-gcp))
    (2 (fig//callout-hexamedia))
    (3 (fig//callout-uwoomfie))
    (9 (fig/ldq))
    (t (fig//friend-set-state 'jumping))))

(defun fig//update-friend ()
  "Update \"friend\"'s state per tick."
  (setf fig//friend-animation (% (+ fig//friend-animation 1) 2))
  (if (> fig//friend-state-timer 0)
      (cl-decf fig//friend-state-timer)
    (setf fig//friend-state 'default))
  (if (> fig//friend-speech-timer 0)
      (cl-decf fig//friend-speech-timer))
  (when (= (random 120) 0)
    (fig//friend-random-event))
  (cl-case fig//friend-state
    (eating (setf fig//friend-state 'eating0))
    (eating0 (setf fig//friend-state 'eating1))
    (eating1 (setf fig//friend-state 'eating2))
    (eating2 (setf fig//friend-state 'eating1))

    (chatting (setf fig//friend-state 'chatting0))
    (chatting0 (setf fig//friend-state 'chatting))
    ))

(defun fig//render-friend ()
  "Render the \"friend\" buffer."
  (save-excursion
    (with-current-buffer (fig//get-friend-buffer)
      (setq-local cursor-type nil)
      (let*
          ((inhibit-read-only t)
           (face (fig//get-friend-face))
           (bubble (fig//get-friend-bubble)))
        (erase-buffer)
        (fig//write
         (format-spec
          "%a\
  /----\\  
 / %l  %r \\ 
 \\  %m  /
  +----+\
"
;;           "%a\
;;     /\\      
;;    / *\\     
;;   / *  \\    
;;  / *  * \\   
;; ----------   
;;  / %l  %r \\ 
;;  \\  %m  /
;;   +----+\
;; "
;;           "%a\
;;     ---       
;;    /   \\     
;;   / [=] \\    
;; -----------   
;;  / %l  %r \\ 
;;  \\  %m  /
;;   +----+\
;; "
          `((?a . ,(s-repeat (fig//get-friend-offset) "          \n"))
            (?l . ,(car face))
            (?r . ,(cadr face))
            (?m . ,(caddr face)))))
        (goto-char (point-min))
        (end-of-line)
        (fig//write (or bubble ""))
        (forward-line)
        (end-of-line)
        (fig//write (if bubble "/" ""))
        ))))

(defvar fig//friend-timer nil)
(defun fig//run-friend-timer ()
  "Run the \"friend\" timer."
  (when fig//friend-timer
    (cancel-timer fig//friend-timer))
  (fig//update-friend)
  (fig//render-friend)
  (setq
   fig//friend-timer
   (run-with-timer 1 nil #'fig//run-friend-timer)))

(defun fig/start-friend ()
  "Launch \"friend\"."
  (interactive)
  (fig//run-friend-timer))

(defun fig/stop-friend ()
  "Stop \"friend\"."
  (interactive)
  (cancel-timer fig//friend-timer)
  (message "\"friend\" is going to sleep!"))

(provide 'fig-friend)
;;; fig-friend.el ends here
