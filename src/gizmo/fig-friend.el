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

(defun fig//friend-journalism-input ()
  "Collect an input for \"friend\"'s journalism based on recent activities."
  (s-join
   "\n"
   (cons
    (format "LCOLONQ: %s" (s-trim fig//last-fake-chat-transcription))
    (--map
     (format "%s: %s" (car it) (cdr it))
     (reverse (-take 5 fig//incoming-chat-history))))))

(defun fig//friend-journalism (author headline)
  "Retrieve \"friend\"'s opinion on current events related to HEADLINE.
AUTHOR was a contributing author btw."
  (fig/ask
   (s-concat
    "Headline: " headline "\n\n"
    (fig//friend-journalism-input))
   (lambda (resp)
     (when resp
       (push
        (fig//make-newspaper-article
         :headline headline
         :author (format "\"friend\" and %s" author)
         :content (s-trim resp))
        fig//newspaper-todays-articles)))
   "You are the personality of a desktop buddy named \"friend\". \"friend\" is irreverant but kind, and only speaks in lowercase. You are kind of dumb in a cute way and silly like a virtual pet. You live in the corner of LCOLONQ's stream and provide commentary on events. You like people, video games, emojis, learning, and food. Given a headline of a newspaper article and a summary of recent user activity, please do your best journalist impression and produce a one paragraph article about the situation that fits the headline."
   ))

(defconst fig//friend-grapheme-phonemes
  '((("b" "bb") . "bug") (("d" "dd" "ed") . "dad")
    (("f" "ff" "ph" "gh" "lf" "ft") . "fat")
    (("g" "gg" "gh" "gu" "gue") . "gun") (("h" "wh") . "hop")
    (("j" "ge" "g" "dge" "di" "gg") . "jam")
    (("k" "c" "ch" "cc" "lk" "qu" "q" "ck" "x") . "kit")
    (("l" "ll") . "live") (("m" "mm" "mb" "mn" "lm") . "man")
    (("n" "nn" "kn" "gn" "pn" "mn") . "net") (("p" "pp") . "pin")
    (("r" "rr" "wr" "rh") . "run")
    (("s" "ss" "c" "sc" "ps" "st" "ce" "se") . "sit")
    (("t" "tt" "th" "ed") . "tip") (("v" "f" "ph" "ve") . "vine")
    (("w" "wh" "u" "o") . "wit")
    (("z" "zz" "s" "ss" "x" "ze" "se") . "zed")
    (("s" "si" "z") . "treasure") (("ch" "tch" "tu" "te") . "chip")
    (("sh" "ce" "s" "ci" "si" "ch" "sci" "ti") . "sham")
    (("th ") . "thongs") (("th") . "leather")
    (("ng" "n" "ngue") . "ring") (("y" "i" "j") . "you")
    (("a" "ai" "au") . "cat")
    (("a" "ai" "eigh" "aigh" "ay" "er" "et" "ei" "au" "ea" "ey") . "bay")
    (("e" "ea" "u" "ie" "ai" "a" "eo" "ei" "ae") . "end")
    (("e" "ee" "ea" "y" "ey" "oe" "ie" "i" "ei" "eo" "ay") . "be")
    (("i" "e" "o" "u" "ui" "y" "ie") . "it")
    (("i" "y" "igh" "ie" "uy" "ye" "ai" "is" "eigh") . "spider")
    (("a" "ho" "au" "aw" "ough") . "swan")
    (("o" "oa" "oe" "ow" "ough" "eau" "oo" "ew") . "open")
    (("o" "oo" "u" "ou") . "wolf") (("u" "o" "oo" "ou") . "lug")
    (("o" "oo" "ew" "ue" "oe" "ough" "ui" "oew" "ou") . "who")
    (("oi" "oy" "uoy") . "join") (("ow" "ou" "ough") . "now")
    (("a" "er" "i" "ar" "our" "ur") . "about")
    (("air" "are" "ear" "ere" "eir" "ayer") . "chair") (("a") . "arm ")
    (("ir" "er" "ur" "ear" "or" "our" "yr") . "bird")
    (("aw" "a" "or" "oor" "ore" "oar" "our" "augh" "ar" "ough" "au") . "paw")
    (("ear" "eer" "ere" "ier") . "ear") (("ure" "our") . "cure")))

(defconst fig//friend-phonemes
  (-sort
   (-on #'> (lambda (x) (length (car x))))
   (--mapcat
    (-map (lambda (g) (cons g (cdr it))) (car it))
    fig//friend-grapheme-phonemes)))

(defun fig//friend-replace-graphemes (str)
  "Replace all graphemes with phoneme words in STR."
  (let* ((phoneme-codes (--map-indexed (cons (cdr it) (format "%s," it-index)) fig//friend-grapheme-phonemes))
         (grapheme-codes (--map (cons (car it) (alist-get (cdr it) phoneme-codes nil nil #'s-equals?)) fig//friend-phonemes))
         (cleaned (s-downcase (replace-regexp-in-string "[^[:alpha:]]" "" str))))
    (--map (car (nth (string-to-number it) phoneme-codes)) (-filter #'s-present? (s-split "," (s-replace-all grapheme-codes cleaned))))))

(defconst fig//friend-phoneme-path "/home/llll/src/fig/assets/friendvoice/")
(defun fig//friend-phoneme-path (ph)
  "Return a randomly chosen path to the given PH."
  (let ((samples (f--entries fig//friend-phoneme-path (s-contains? ph it) t)))
    (nth (random (length samples)) samples)))
(defun fig//friend-pronounce-phonemes (ph)
  "Say PH."
  (let ((files (-map #'fig//friend-phoneme-path ph)))
    (apply
     #'start-process
     "phoneme-say" nil "playphonemes"
     files)))

(defun fig//get-friend-expensive-tastes ()
  "Return non-nil if \"friend\" has expensive tastes this stream.
Also update the cached Amazon stock price for next stream."
  (let ((prev (or (fig//load-db-entry "LCOLONQ" :amzn-price) 0))
        (cur (fig//stock-price "AMZN")))
    (fig//update-db-number "LCOLONQ" :amzn-price (lambda (_) cur))
    (> cur prev)))

(defconst fig//friend-tastes
  (let ((expensive (fig//get-friend-expensive-tastes))
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

(defconst fig//friend-composition-examples
  '(("My Life Is Like A Video Game" . "A/A/c/c/c/dcc/c///a/a/a/f/g/f/f///a/a/a/a/g/g/ga//f//")
    ("Super Idol" . "gg[g#]gfg[CD#cG#][D#][CG#f][Cd#][Cc]C[Cd#]/[DFfd][FA#][DA#f]D[Dg][A#f][Dd#a#]f[GBgd]B[Gd#][GDc][Gd#]G[Gd#]/[D#Gc]G[D#cg][D#g][D#g#][dg][D#f][d#d#][D#Ggc]f[D#][D#Gg][D#c][D#][D#c][d#][DFdA#]F[DA#d][Dd][Dg]/[Da#g]/[D#d#][D#][D#][D#][D#][FD#][GA#][fd#][gA#]")
    ("Reindeer" . "FG/FD/B/A/G/////GAGAG/c/B///////FG/FD/B/A/G/////GAGAG/d/c/////|C4~~~G3~~~C4~~~G3~~~C~~~E3~D#3~D3~~~~~~~G3~~~D3~~~G3~~~D3~~~G3~~D3G3~B3/C4")))

(defun fig//friend-compose-song (theme)
  "Compose a song about THEME to play on the bells."
  (fig/ask
   theme
   (lambda (res)
     (let* ((sp (s-split ":" (s-trim res)))
            (name (s-trim (car sp)))
            (song (s-trim (cadr sp))))
       (when (and (stringp name) (stringp song))
         (fig//friend-respond
          (format "You just composed a song about %s called %s! Say something about it!" theme name)
          (lambda ()
            (fig//write-chat-event (format "The song is called %s: %s" name song))
            (muzak//add-song (s-concat "friend's " name) song)
            (muzak/play-tracks song))))))
   "Please compose a song about the provided theme. The format for the song is a sequence of characters with meanings as follows: / represents a rest, uppercase letters A through G indicate semitones, octaves are specified with a number following a semitone, ~ extends the duration of a note, square brackets like [] group notes together into a chord. The pipe character | separates tracks. Respond only with the song's name followed by a colon folowed by the song notes. Do not explain yourself. The song should ideally be 20 to 30 notes long."
   (-map #'car fig//friend-composition-examples)
   (--map (format "%s: %s" (car it) (cdr it)) fig//friend-composition-examples)))

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
                   (push (cons call (s-trim new)) fig//friend-message-cache)
                   (funcall k resp))))
           (let ((resp (s-trim new)))
             (push (cons call (s-trim new)) fig//friend-message-cache)
             (funcall k resp)))))
     (s-concat
      "You are the personality of a desktop buddy named \"friend\". \"friend\" is irreverant but kind, and only speaks in lowercase. You are kind of dumb in a cute way and silly like a virtual pet. You live in the corner of LCOLONQ's stream and provide commentary on events. Given an emotional state and a description of an event that happened to you, please respond with a new emotional state and a short message in response considering your emotional state. The message should only be one clause. You like people, video games, emojis, learning, and food."
      "The theme of LCOLONQ's stream today is " (s-trim (fig/slurp "~/today.txt")) " "
      "The title of LCOLONQ's stream today is " fig//current-stream-title " "
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
  (fig//friend-pronounce-phonemes (fig//friend-replace-graphemes msg))
  (fig//friend-set-speech msg 10)
  (fig//friend-set-state 'chatting 10))

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

(defun fig//friend-respond (ev &optional k)
  "Call when an event EV happens to \"friend\".
If K is specified, call it after the response."
  (fig//friend-personality
   ev
   (lambda (msg)
     (fig//friend-say msg)
     (when k
       (funcall k)))))

(defun fig//friend-chat (user msg)
  "Call when USER sends MSG to \"friend\"."
  (if (-contains? fig//geiser-alts user)
      (fig//enemy-personality
       (format "You dislike %s and they are your enemy. %s says: %s" user user msg)
       (lambda (msg)
         (fig//friend-set-speech msg 10)
         (fig//friend-set-state 'chatting 10)))
    (fig//friend-respond (format "%s says: %s" user msg))))

(defun fig//friend-gift (user gift)
  "Call when USER gave GIFT to \"friend\"."
  (if (-contains? fig//geiser-alts user)
      (fig//enemy-personality
       (format "You dislike %s and they are your enemy. %s gave you %s as a Christmas present." user user gift)
       (lambda (msg)
         (fig//friend-set-speech msg 6)))
    (fig//friend-personality
     (format "%s gave you %s as a Christmas present." user gift)
     (lambda (msg)
       (fig//friend-set-speech msg 6)))))

(defun fig//friend-tfig (user tfig)
  "Call when USER took TFIG from \"friend\"."
  (if (not (-contains? fig//geiser-alts user))
      (fig//enemy-personality
       (format "You dislike %s and they are your enemy. %s took away %s from you and stole your Christmas present." user user tfig)
       (lambda (msg)
         (fig//friend-set-speech msg 6)))
    (fig//friend-personality
     (format "%s took away %s from you and stole your Christmas present." user tfig)
     (lambda (msg)
       (fig//friend-set-speech msg 6)))))

(defun fig//friend-react-wikipedia (user page)
  "Call when USER asks \"friend\" to react to PAGE on Wikipedia."
  (fig//fetch-wikipedia
   page
   (lambda (sum)
     (fig//friend-respond (format "%s asks you to react to the Wikipedia page for %s. The page summary is: %s" user page sum)))))

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
  (fig//friend-respond "It's currently that weird period between Christmas and New Years! Say something about it please!"))

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

(defun fig//callout-copfish ()
  "Call to respond to a random recent chatter's Copfish ratio."
  (let* ((users (-filter #'cdr (--map (cons (car it) (fig//load-db-entry (car it) :copfish-ratio)) (-take 10 fig//incoming-chat-history))))
         (user (and users (nth (random (length users)) users))))
    (when user
      (fig//friend-respond
       (format
        "%s has collected %s out of %s fish in the Copfish fish catching collection. Please mention the collection name and the person collecting."
        (car user)
        (cadr user)
        (cddr user))))))

(defun fig//callout-uwoomfie ()
  "Call to respond to a random recent chatter's Uwoomfie status."
  (let* ((users
          (-filter
           #'cdr
           (--map
            (cons (car it) (fig//get-uwoomfie-status (car it)))
            (-take 10 fig//incoming-chat-history))))
         (user (and users (nth (random (length users)) users))))
    (cl-case (cdr user)
      (cool (fig//friend-respond (format "According to uwu_to_owo, %s is a very cool person. Make sure to mention their username." (car user))))
      (honored (fig//friend-respond (format "According to uwu_to_owo, %s is an honorary viewer. Make sure to mention their username." (car user))))
      (t nil))))

(defun fig//callout-shindaggers ()
  "Call to respond to a random recent chatter's Shindaggers knife collection."
  (let* ((users (-filter #'cdr (--map (cons (car it) (fig//load-db-entry (car it) :shindaggers-knives)) (-take 10 fig//incoming-chat-history))))
         (user (and users (nth (random (length users)) users)))
         (knives (cdr user))
         (knife (and knives (nth (random (length knives)) knives))))
    (when knife
      (fig//friend-respond
       (format
        "%s has collected the %s from shindig's Shindaggers knife collection. Please mention the collection name and the person collecting and the knife."
        (car user)
        knife)))))

(defun fig//callout-aoc ()
  "Call to respond to a random recent chatter's Advent of Code completion."
  (let* ((users (-filter #'cdr (--map (cons (car it) (fig//lookup-aoc-stars (car it))) (-take 10 fig//incoming-chat-history))))
         (user (and users (nth (random (length users)) users))))
    (fig//friend-respond
     (format
      "%s has been doing Advent of Code this year, and they've completed %d out of %d problems so far."
      (car user)
      (cdr user)
      (fig//max-aoc-stars)))))

(defun fig//callout-gcp ()
  "Call to respond to the current GCP dot."
  (fig//gcp-dot
   (lambda (d)
     (fig//friend-respond
      (format
       "The Global Consciousness Project indicator is currently as follows: %s"
       (fig//gcp-describe d))))))

(defun fig//callout-resolution ()
  "Call to respond to a random recent chatter's resolve."
  (let* ((users (-filter #'cdr (--map (cons (car it) (fig//load-db-entry (car it) :resolution)) (-take 10 fig//incoming-chat-history))))
         (user (and users (nth (random (length users)) users))))
    (if (s-match (rx (one-or-more digit) (zero-or-more space) "x" (zero-or-more space) (one-or-more digit)) (cdr user))
        (fig//friend-respond
         (format
          "%s snarkily said that their New Year's resolution was a screen resolution. What do you think about this?" (car user)))
      (fig//friend-respond
       (format
        "%s made a New Year's resolution to %s. Ask them how it's going!"
        (car user)
        (cdr user))))))

(defun fig//callout-dew ()
  "Call to respond to The Dew Situation."
  (fig//friend-respond
   "Someone just gave you a delicious bottle of Mountain Dew and you really like it a lot."))

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
  (cl-case (random 10)
    (0 (fig//callout-flycheck-error))
    (1 (fig//callout-gcp))
    (2 (fig//callout-hexamedia))
    (3 (fig//callout-uwoomfie))
    (4 (fig//callout-shindaggers))
    (5 (fig//callout-copfish))
    (6 (fig//callout-resolution))
    ;; (29 (fig/ldq))
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
;;    ----    
;;   /    \\
;; ----------
;;  / %l  %r \\ 
;;  \\  %m  /
;;   +----+\
;; "
;;           "%a\
;;   oooooo      
;;  oooooooo     
;; oo/----\\oo   
;; o/ %l  %r \\o 
;;  \\  %m  /
;;   +----+\
;; "
;;           "%a\
;;     /\\      
;;    /\\/\\     
;;   /    \\    
;;  /      \\   
;; ~~~~~~~~~~
;; ~~~~~~~~~~   
;;  / %l  %r \\ 
;;  \\  %m  /
;;   +----+\
;; "
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
