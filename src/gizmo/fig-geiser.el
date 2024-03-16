;;; fig-geiser --- Geiser counter and factions -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'dash)
(require 's)
(require 'f)
(require 'ht)
(require 'cl-lib)
(require 'request)
(require 'lunar)

(defvar fig//super-idol-tally 0)
(defun fig//check-super-idol-tally ()
  "Check the Super Idol tally and play SILGC if appropriate."
  (when (>= fig//super-idol-tally 20)
    (soundboard//play-clip "silgc.ogg")
    (setf fig//super-idol-tally 0)))

(defvar fig//geiser-alts (list "imgeiser" "notgeiser" "cbtcaptain" "mahjongpilled" "infant_yeetah" "mindgoblindeeznutslmao"))

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

(defvar fig//times-asked-about-heartrate 0)
(defun fig/increment-heartrate-counter ()
  "Increment the heartrate question counter by 1."
  (interactive)
  (cl-incf fig//times-asked-about-heartrate))

(defun fig//get-load ()
  "Get the current CPU load."
  (let ((res (shell-command-to-string "uptime")))
    (string-to-number (s-trim (car (s-split "," (cadr (s-split "load average:" res))))))))

(defun fig//get-heartrate ()
  "Get the streamer's heart rate."
  (* 100 (fig//get-load)))

(defface fig/heartrate-big
  '((t
     :foreground "white"
     :height 700
     ))
  "Face for big heartrate."
  :group 'fig)

(defface fig/heartrate-small
  '((t
     :foreground "white"
     ))
  "Face for small heartrate disclaimer."
  :group 'fig)

(defcustom fig/heartrate-buffer "*fig-heartrate*"
  "Name of buffer used to display heartrate."
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
      (fig//write-line (format "%3d bpm" (fig//get-heartrate)) 'fig/heartrate-big)
      (fig//write (format "arbitrary counter: %s times" fig//times-asked-about-heartrate) 'fig/heartrate-small))))

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

(defun fig//random-steampunk-name (name k)
  "Pass a random steampunk version of NAME to K."
  (fig/ask
   name
   (lambda (d)
     (funcall k (s-trim d)))
   "Generate a random steampunk style version of the provided name. The output should be only a single first name."
   "Steven"
   "Steamven"))

(defvar fig//steampunk-names nil)
(defun fig//populate-steampunk-names (names)
  "Populate `fig//steampunk-names' from NAMES."
  (unless (null names)
    (fig//random-steampunk-name
     (car names)
     (lambda (sn)
       (push sn fig//steampunk-names)
       (message "added steampunk name: %s" sn)
       (fig//populate-steampunk-names (cdr names))))))

(defun fig//random-magic-deck (name k)
  "Given a NAME, produce a Legacy format magic the gathering deck and pass it to K."
  (fig/ask
   name
   (lambda (d)
     (funcall k (s-trim d)))
   "Create a Magic: The Gathering decklist for the Legacy format based on the given name."
   "Tin Fins"
   "Tin Fins
1 Children of Korlis
2 Griselbrand
1 Emrakul, the Aeons Torn
4 Lotus Petal
4 Shallow Grave
3 Daze
2 Goryo's Vengeance
2 Lim-Dûl's Vault
4 Dark Ritual
4 Brainstorm
4 Entomb
4 Force of Will
1 Consider
4 Ponder
1 Cabal Therapy
3 Thoughtseize
2 Preordain
1 Island
1 Swamp
4 Underground Sea
2 Flooded Strand
4 Polluted Delta
2 Verdant Catacombs

Sideboard
1 Edge of Autumn
2 Street Wraith
1 Shelldock Isle
1 Personal Tutor
1 Cavern of Souls
1 Echoing Truth
1 Lion's Eye Diamond
2 Flusterstorm
4 Doomsday
1 Thassa's Oracle
"))

(define-derived-mode fig/clonkhead-io-mode special-mode "ClonkHead Stats"
  "Major mode for displaying Clonk Head statistics."
  :group 'fig
  (setq mode-line-format nil))

(defun fig//get-clonkhead-io-buffer (user)
  "Return the stats buffer for USER."
  (let ((name (format "*fig-clonkhead %s*" user)))
    (unless (get-buffer name)
      (with-current-buffer (get-buffer-create name)
        (fig/clonkhead-io-mode)))
    (get-buffer name)))

(defface fig/clonkhead-io-title
  '((t
     :foreground "white"
     :height 300
     ))
  "Face for title."
  :group 'fig)

(defface fig/clonkhead-io-category
  '((t
     :foreground "green"
     ))
  "Face for title."
  :group 'fig)

(defvar fig//element-display-info
  '(("fire" "🔥" "red")
    ("water" "🌊" "blue")
    ("wind" "🍃️" "green")
    ("earth" "🪨" "brown")
    ("lightning" "⚡" "yellow")
    ("heart" "🩷" "pink")
    ))
(defun fig//display-element (e)
  "Return a propertized string representing E."
  (if-let ((dinfo (alist-get e fig//element-display-info nil nil #'s-equals?)))
      (propertize
       (format "%s %s" (car dinfo) e)
       'face (list :foreground (cadr dinfo)))
    "O.O unknown?"))
(defun fig//get-chatter-element (user)
  "Get the element for USER."
  (fig//load-db-entry user :element))
(defun fig//assign-chatter-element (user)
  "If USER doesn't have an element, assign them an element."
  (unless (fig//get-chatter-element user)
    (fig//update-db-default
     user
     :element
     (lambda (f)
       (if (and f (symbolp f)) f
         (let* ((elements '("fire" "water" "wind" "earth" "lightning" "heart")))
           (nth (random (length elements)) elements))))
     nil)))

(defconst fig//common-first-names (-filter #'s-present? (s-split "\n" (fig/slurp "~/src/fig/assets/names.txt"))))
(defconst fig//common-last-names (-filter #'s-present? (s-split "\n" (fig/slurp "~/src/fig/assets/lastnames.txt"))))
(defun fig//get-chatter-identity (user)
  "Get the identity for USER."
  (fig//load-db-entry user :identity))
(defun fig//assign-chatter-identity (user)
  "If USER doesn't have an identity, assign them an identity."
  (unless (fig//get-chatter-identity user)
    (fig//update-db-default
     user
     :identity
     (lambda (f)
       (if (and f (stringp f)) f
         (format
          "%s %s"
          (nth (random (length fig//common-first-names)) fig//common-first-names)
          (nth (random (length fig//common-last-names)) fig//common-last-names))))
     nil)))
(defvar fig//identity-chat-toggle nil)
(defun fig//toggle-chat-identity ()
  "Toggle the display of real legal names in chat."
  (interactive)
  (setq fig//identity-chat-toggle (not fig//identity-chat-toggle)))

(defun fig//render-clonkhead-io (user)
  "Render the stats buffer for USER."
  (with-current-buffer (fig//get-clonkhead-io-buffer user)
    (let* ((inhibit-read-only t)
           (faction (fig//get-chatter-faction user))
           (element (fig//get-chatter-element user))
           (identity (fig//get-chatter-identity user))
           (boosts (fig//load-db-entry user :boost))
           (anc (fig//get-chatter-ancestor user))
           (char (fig//get-chatter-character user))
          )
      (erase-buffer)
      (fig//write-line user 'fig/clonkhead-io-title)
      (fig//write-line (format "Name: %s" identity))
      (fig//write
       (format
        "Faction: %s"
        (propertize
         (format "%s" (or faction "EXEMPT"))
         'face
         (list
          :foreground
          (cl-case faction
            (nate "pink")
            (lever "lightblue")
            (tony "lightgreen")
            (t "white"))))))
      (fig//write-line
       (cond
        ((not boosts) " (objector)")
        ((> boosts 0) (format " (boost %s)" boosts))
        (t (format " (%s tsoob)" boosts))))
      (fig//write-line
       (format
        "Element: %s"
        (fig//display-element element)))
      (when char
        (let ((scores (fig//rpg-character-ability-scores char)))
          (fig//write-line (format "Class: %s" (fig//rpg-class-name (fig//rpg-character-class char))))
          (fig//write-line (format "HP: %s" (fig//rpg-character-max-hp char)))
          (fig//write-line
           (format
            "Stats:\n  STR %s DEX %s CON %s\n  INT %s WIS %s CHA %s"
            (fig//rpg-ability-scores-str scores)
            (fig//rpg-ability-scores-dex scores)
            (fig//rpg-ability-scores-con scores)
            (fig//rpg-ability-scores-int scores)
            (fig//rpg-ability-scores-wis scores)
            (fig//rpg-ability-scores-cha scores)
            ))))
      (when anc
        (fig//write-line (format "Ancestor:\n  %s" (fig//ancestor-name anc)))
        )
      (goto-char (point-min))
      )
    ))

(defvar fig//clonkhead-io-frame nil)
(defvar fig//clonkhead-io-cur nil)
(defun fig//create-clonkhead-io-frame ()
  "Build a frame for displaying ClonkHead stats on mouseover."
  (when (framep fig//clonkhead-io-frame)
    (delete-frame fig//clonkhead-io-frame))
  (setf
   fig//clonkhead-io-frame
   (make-frame
    (append
     `((name . "clonkhead-io")
       (fig-prevent-focus . t)
       (unsplittable . t)
       (undecorated . t)
       (no-accept-focus . t)
       (no-focus-on-map . t)
       (override-redirect . t)
       (user-size . t)
       (width . 30)
       (height . 15)
       (user-position . t)
       (left . -1)
       (top . -1)
       (default-minibuffer-frame . ,(selected-frame))
       (minibuffer . nil)
       (left-fringe . 0)
       (right-fringe . 0)
       (cursor-type . nil)
       (background-color . "black"))))))
(defun fig//display-clonkhead-io (user &optional x y)
  "Display the ClonkHead IO buffer for USER.
Optionally display the window at X, Y"
  (unless fig//clonkhead-io-frame
    (fig//create-clonkhead-io-frame))
  (let ((window (frame-selected-window fig//clonkhead-io-frame)))
    (if (and x y)
        (fig//move-clonkhead-io-frame x y)
      (fig//move-clonkhead-io-frame -1 -1))
    (fig//render-clonkhead-io user)
    (setq fig//clonkhead-io-cur user)
    (set-window-buffer window (fig//get-clonkhead-io-buffer user))
    (fig//show-clonkhead-io-frame t)))
(defun fig//show-clonkhead-io-frame (vis)
  "If VIS is non-nil, make the ClonkHead IO frame visible.
Otherwise make it invisible."
  (if vis
      (make-frame-visible fig//clonkhead-io-frame)
    (setq fig//clonkhead-io-cur nil)
    (make-frame-invisible fig//clonkhead-io-frame)))
(defun fig//move-clonkhead-io-frame (x y)
  "Move the ClonkHead IO frame to X, Y."
  (modify-frame-parameters
   fig//clonkhead-io-frame
   (list
    (cons 'top y)
    (cons 'left x))))
(defun fig//update-clonkhead-io (user pos)
  "Update the ClonkHead IO frame for USER based on POS."
  (if (and user pos)
      (progn
        (unless (equal (cons user pos) fig//clonkhead-io-cur)
          (fig//display-clonkhead-io user (car pos) (cdr pos)))
      )
    (fig//show-clonkhead-io-frame nil)))
(defun fig//handle-clonkhead-io ()
  "Handle point movement for ClonkHead IO popup."
  (with-current-buffer (fig//get-twitch-chat-buffer)
    (fig//update-clonkhead-io
     (get-text-property (point) 'fig-user)
     (window-absolute-pixel-position (point)))))
(fig//create-clonkhead-io-frame)
(fig//show-clonkhead-io-frame nil)

(defconst fig//8ball-answers
  '((1 "It is certain" "It is decidedly so" "Without a doubt" "Yes definitely" "You may rely on it")
    (2 "As I see it, yes" "Most likely" "Outlook good" "Yes" "Signs point to yes")
    (3 "Reply hazy, try again" "Ask again later" "Better not tell you now" "Cannot predict now" "Concentrate and ask again")
    (4 "Don't count on it" "My reply is no" "My sources say no" "Outlook not so good" "Very doubtful")
    (5 "Joel" "You are a silly goose (barnacle)" "Bet" "It's so over" "We're so back" "Commit spontaneous generation")))
(defun fig//8ball (query k)
  "Consult the magic conch for QUERY. Pass the resulting answer K."
  (fig/ask
   query
   (lambda (res)
     (let* ((num (string-to-number (s-trim res)))
            (answers (alist-get (if (or (< num 1) (> num 5)) (random 6) num) fig//8ball-answers nil nil #'=))
           )
       (funcall
        k
        (nth (random (length answers)) answers)
        )))
   "Given the user's query, respond with a single number between 0 and 4 based on how likely the query is to be true. 1 is more likely, 5 is least likely."
   (list "Is the sky blue?" "Is today Friday?" "How to shot web?")
   (list "1" "3" "5")
   ))

(provide 'fig-geiser)
;;; fig-geiser.el ends here
