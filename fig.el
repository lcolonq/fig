;;; fig --- Pub/sub bus client -*- lexical-binding: t; -*-
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

(defcustom fig/twitch-chat-buffer "*fig-chat*"
  "Name of buffer used to store the chat log."
  :type '(string)
  :group 'fig)

(defvar fig/quotes nil)
(defun fig//save-quotes ()
  "Save the quotes database."
  (fig//save-db "__QUOTES__" fig/quotes))
(defun fig//load-quotes ()
  "Load the quotes database."
  (setf fig/quotes (fig//load-db "__QUOTES__")))
(defun fig//add-quote (user q)
  "Add quote Q from USER."
  (add-to-list 'fig/quotes (cons q user))
  (fig//save-quotes))
(fig//load-quotes)

(defvar fig/recommended-books nil)
(defun fig//save-recommended-books ()
  "Save the quotes database."
  (fig//save-db "__BOOKS__" fig/recommended-books))
(defun fig//load-recommended-books ()
  "Load the quotes database."
  (setf fig/recommended-books (fig//load-db "__BOOKS__")))
(defun fig//add-recommended-book (user b)
  "Add book B from USER."
  (add-to-list 'fig/recommended-books (cons b user))
  (fig//save-recommended-books))
(fig//load-recommended-books)

(defvar fig//assess-chat-spirituality t
  "Whether or not to print Bible word summary in chat messages.")

(defvar fig//current-poll-callback nil
  "A callback that is called and passed the poll winner when the poll concludes.")

(defvar fig//current-prediction-ids nil
  "Prediction and outcome identifiers for the current prediction.")

;; (defconst fig/host "shiro")
(defconst fig/host "localhost")
(defconst fig/port 32050)

(defconst fig//event-handlers
  (list
   ;; (cons '(bald stat total)
   ;;       (lambda (msg)
   ;;         (let ((val (car msg)))
   ;;           (when (< val 90)
   ;;             (fig//baldur-click 700 710)
   ;;             (fig//baldur-cmd "check")))))
   (cons '(monitor twitch chat incoming) #'fig//handle-twitch-message)
   (cons '(monitor discord chat incoming) #'fig//handle-discord-message)
   (cons '(monitor irc chat incoming) #'fig//handle-discord-message)
   (cons '(monitor twitch redeem incoming) #'fig//handle-redeem)
   (cons '(monitor twitch poll begin)
         (lambda (_)
           (fig//write-chat-event "Poll started")
           (fig//friend-respond "The chatters are doing a poll")))
   (cons '(monitor twitch poll end)
         (lambda (msg)
           (let ((winner (car (-max-by (-on #'> #'cadr) (cadr msg)))))
             (fig//write-chat-event (format "Poll finished, winner is: %s" winner))
             (when fig//current-poll-callback
               (funcall fig//current-poll-callback winner))
             (setq fig//current-poll-callback nil))))
   (cons '(monitor twitch prediction begin)
         (lambda (msg)
           (fig//write-chat-event "Gamble started")
           (fig//friend-respond "The chatters are gambling")
           (setq fig//current-prediction-ids msg)))
   (cons '(monitor twitch prediction end)
         (lambda (_)
           (fig//write-chat-event "Gamble finished")
           (setq fig//current-prediction-ids nil)))
   (cons '(monitor twitch raid)
         (lambda (msg)
           (let ((user (car msg)))
             (soundboard//play-clip "rampage.mp3")
             (fig//write-chat-event (format "%s just raided!" user))
             (fig//friend-respond (format "%s just came to visit" user))
             (run-with-timer
              15 nil
              (lambda ()
                (fig//twitch-spotlight-streamer user))))))
   (cons '(monitor twitch follow)
         (lambda (msg)
           (let ((user (car msg)))
             (soundboard//play-clip "firstblood.mp3")
             (fig//model-region-word "skin" (format "welcome_%s_" user))
             (fig//friend-respond (format "%s just followed the stream" user))
             (fig//write-chat-event (format "New follower: %s" user)))))
   (cons '(monitor twitch subscribe)
         (lambda (msg)
           (let ((user (car msg)))
             (fig//rats-rats-we-are-the-rats user)
             (fig//model-region-word "skin" (format "thanks_%s_" user))
             (fig//friend-respond (format "%s just subscribed to the stream" user))
             (fig//write-chat-event (format "New subscriber: %s" user)))))
   (cons '(monitor twitch gift)
         (lambda (msg)
           (let ((user (car msg))
                 (subs (cadr msg)))
             (fig//model-region-word "skin" (format "GIGACHAD_%s_" user))
             (fig//friend-respond (format "%s just gifted subscriptions" user))
             (fig//write-chat-event (format "%s gifted %d subs" user subs))
             (soundboard//play-monsterkill subs))))
   (cons '(monitor twitch cheer)
         (lambda (msg)
           (let ((user (car msg))
                 (bits (cadr msg)))
             (fig//friend-respond (format "%s just donated money" user))
             (fig//write-chat-event (format "%s cheered %d bits" user bits))
             (fig//twitch-say
              (format
               "@%s here's one character of my stream key: %c"
               user
               (seq-elt fig//shuffled-stream-key (random (seq-length fig//shuffled-stream-key))))))))
   ))

(defconst fig//twitch-chat-commands
  (list
   (cons "MRBEAST" (lambda (_ _) (soundboard//play-clip "mrbeast.mp3")))
   (cons "NICECOCK" (lambda (_ _) (soundboard//play-clip "pantsintoashes.mp3")))
   (cons "hexadiCoding" (lambda (_ _) (soundboard//play-clip "developers.ogg")))
   (cons "roguelike" (lambda (user _) (fig//twitch-say (format "@%s that's not a roguelike" user))))
   (cons "arch btw" (lambda (_ _) (fig//twitch-say "I use nix btw")))
   (cons "!commands"
         (lambda (_ _)
           (fig//twitch-say
            (s-concat
             "Available commands: "
             (s-join " " (--filter (s-contains? "!" it) (-map #'car fig//twitch-chat-commands)))))))
   (cons "!today"
         (lambda (_ _)
           (fig//twitch-say
            (s-trim
             (with-temp-buffer
               (insert-file-contents-literally "~/today.txt")
               (buffer-string))))))
   (cons "!nc" (lambda (_ _) (fig//twitch-say "try: \"nc colonq.computer 31340\", if nc doesn't work try ncat or telnet")))
   (cons "!oomfie" (lambda (_ _) (fig//twitch-say "hi!!!!!!!")))
   (cons "!forth" (lambda (_ _) (fig//twitch-say "https://gforth.org")))
   (cons "!game" (lambda (_ _) (fig//twitch-say "https://oub.colonq.computer")))
   (cons "!webring" (lambda (_ _) (fig//twitch-say "https://pub.colonq.computer")))
   (cons "!faction"
         (lambda (user _)
           (fig//twitch-say (format "faction for %s: %s" user (fig//get-chatter-faction user)))))
   (cons "!lore"
         (lambda (_ _)
           (fig/ask
            "ITEM"
            (lambda (msg) (fig//twitch-say msg))
            "Please produce a Dark Souls style item name and description related to LCOLONQ. Please limit your response to one sentence maximum. The sentence should be vague and incorporate archaic words that are not commonly used. LCOLONQ is a spirit that lives inside the computer. LCOLONQ is associated with: the moon, snakes, the color grey, dolls and puppets, amber, the wind, and GNU Emacs. The description should mostly describe the item, but with vague insinuations about the true nature of LCOLONQ."
            "ITEM"
            "Ring of Favor and Protection - A ring symbolizing the favor and protection of the goddess Fina, known in legend to possess fateful beauty."
            )))
   (cons "!geisercounter" (lambda (_ _) (fig//twitch-say (format "The Geiser counter beeps %s times" (fig//geiser-counter)))))
   (cons "!bible" (lambda (_ _) (fig//twitch-say "https://www.youtube.com/watch?v=G5u23bh29hI")))
   (cons "!drink" (lambda (_ _) (fig//twitch-say "its watah im drinkin it")))
   ;; (cons "!palettes"
   ;;       (lambda (_ _)
   ;;         (fig//twitch-say
   ;;          (format
   ;;           "Available palettes: %s"
   ;;           (s-join " " (-map #'fig//palette-name fig/palettes))))))
   ;; (cons "!palette"
   ;;       (lambda (_ inp)
   ;;         (when-let*
   ;;             ((trimmed (s-trim (s-replace "!palette" "" inp)))
   ;;              (pal (fig//get-palette trimmed)))
   ;;           (fig//twitch-say
   ;;            (format "%s" (s-join " " (fig//write-palette pal)))))))
   (cons "!bookrec"
         (lambda (_ _)
           (let ((choice (nth (random (length fig/recommended-books)) fig/recommended-books)))
             (fig//twitch-say (format "%s (recommended by %s)" (car choice) (cdr choice))))))
   (cons "!addbookrec"
         (lambda (user inp)
           (let ((trimmed (s-trim (s-replace "!addbookrec" "" inp))))
             (fig//write-chat-event (format "%s recommends: %s" user trimmed))
             (fig//add-recommended-book user trimmed))))
   (cons "!quote"
         (lambda (_ _)
           (let ((choice (nth (random (length fig/quotes)) fig/quotes)))
             (fig//twitch-say (format "%s: %s" (cdr choice) (car choice))))))
   (cons "!addquote"
         (lambda (user inp)
           (let ((trimmed (s-trim (s-replace "!addquote" "" inp))))
             (fig//write-chat-event (format "%s saves quote: %s" user trimmed))
             (fig//add-quote user trimmed))))
   (cons "!twitter"
         (lambda (_ _)
           (fig/ask "How do you feel about Twitter? Should viewers follow LCOLONQ on Twitter?" #'fig/say)
           (fig//twitch-say "https://twitter.com/LCOLONQ")))
   (cons "discord" (lambda (_ _) (fig//twitch-say "https://discord.gg/f4JTbgN7St")))
   (cons "Discord" (lambda (_ _) (fig//twitch-say "https://discord.gg/f4JTbgN7St")))
   (cons "!irc" (lambda (_ _) (fig//twitch-say "#cyberspace on IRC at colonq.computer:26697 (over TLS)")))
   (cons "IRC" (lambda (_ _) (fig//twitch-say "#cyberspace on IRC at colonq.computer:26697 (over TLS)")))
   (cons "!sponsor" (lambda (_ _) (fig//twitch-say "Like what you see? Don't forget to download GNU Emacs at https://www.gnu.org/software/emacs/?code=LCOLONQ")))
   (cons "!specs" (lambda (_ _) (fig//twitch-say "Editor: evil-mode, WM: EXWM, OS: NixOS, hardware: shit laptop")))
   (cons "!coverage" (lambda (_ _) (fig//twitch-say (s-concat "Test coverage: " (number-to-string (random 100)) "%"))))
   (cons "!learnprogramming" (lambda (_ _) (fig//twitch-say "1) program")))
   (cons "!github" (lambda (_ _) (fig//twitch-say "https://github.com/lcolonq")))
   (cons "!language" (lambda (_ _) (fig//twitch-say "probably emacs lisp or maybe rust")))
   (cons "!onlyfans" (lambda (_ _) (soundboard//play-clip "pornhub.mp3")))
   (cons "!throne" (lambda (_ _) (fig//twitch-say "xdding")))
   (cons "!vim" (lambda (_ _) (fig//twitch-say "vi is the best text editor, emacs is the best operating system")))
   (cons "!emacs" (lambda (_ _) (fig//twitch-say "i've tried everything else emacs is best girl")))
   (cons "!fish" (lambda (_ _) (fig//twitch-say "Not even a nibble...")))
   (cons "!roll" (lambda (user _) (fig//twitch-say (fig//character-to-string (fig//roll-character user)))))
   (cons
    "!leaderboard"
    (lambda (_ _)
      (let* ((users (fig//all-db-users))
             (user-scores (-filter #'cdr (--map (cons it (alist-get :boost (fig//load-db it))) users)))
             (sorted (-sort (-on #'> #'cdr) user-scores))
             (leaders (-take 5 sorted)))
        (fig//twitch-say (s-join ", " (--map (format "%s: %s" (car it) (cdr it)) leaders))))))
   (cons
    "draobredael!"
    (lambda (_ _)
      (let* ((users (fig//all-db-users))
             (user-scores (-filter #'cdr (--map (cons it (alist-get :boost (fig//load-db it))) users)))
             (sorted (-sort (-on #'< #'cdr) user-scores))
             (leaders (-take 5 sorted)))
        (fig//twitch-say (s-join ", " (--map (format "%s: %s" (reverse (car it)) (cdr it)) leaders))))))
   (cons
    "!vippers"
    (lambda (_ _)
      (let ((vipperstring (s-join ", " (fig//shuffle-seq fig//twitch-vip-list))))
        (fig//twitch-say (seq-take vipperstring 450)))
      (fig//twitch-get-vip-list)))
   (cons "!levelup"
         (lambda (user _)
           (fig//update-db-character
            user
            (lambda (c)
              (cl-incf (fig//rpg-character-level c))
              c))
           (fig//twitch-say (fig//character-to-string (fig//get-db-character user)))))
   ))

(defvar fig//current-strength 0
  "My current strength.")

(defun fig//render-strength ()
  "Display strength."
  (with-current-buffer (get-buffer-create "*fig-strength*")
    (erase-buffer)
    (fig//write-line (format "Current strength: %s/511" fig//current-strength))))

(defconst fig//twitch-redeems
  (list
   (cons "mental clarity"
         (lambda (user _)
           (fig//write-chat-event (format "%s established mental clarity" user))
           (fig/mental-clarity)))
   (cons "theme: autumn"
         (lambda (user _)
           (fig//write-chat-event (format "%s changed the theme: autumn" user))
           (fig//change-theme 'ef-autumn)))
   (cons "theme: tritanopia-dark"
         (lambda (user _)
           (fig//write-chat-event (format "%s changed the theme: tritanopia-dark" user))
           (fig//change-theme 'ef-tritanopia-dark)))
   (cons "theme: duo-dark"
         (lambda (user _)
           (fig//write-chat-event (format "%s changed the theme: duo-dark" user))
           (fig//change-theme 'ef-duo-dark)))
   (cons "theme: bio"
         (lambda (user _)
           (fig//write-chat-event (format "%s changed the theme: bio" user))
           (fig//change-theme 'ef-bio)))
   (cons "decorate room"
         (lambda (user inp)
           (let* ((sp (s-split " " (s-trim inp)))
                  (emote (car sp))
                  (xs (cadr sp))
                  (ys (caddr sp)))
             (fig//write-chat-event (format "%s decorated: %s" user inp))
             (fig//mansion-place-emote-cell
              emote
              (if xs (string-to-number xs) (random 4))
              (if ys (string-to-number ys) (random 4))
              (lambda () nil)))))
   (cons "carve pumpkin"
         (lambda (user inp)
           (let* ((sp (s-split " " (s-trim inp)))
                  (emote (car sp))
                  (xs (cadr sp))
                  (ys (caddr sp)))
             (fig//write-chat-event (format "%s carved the pumpkin: %s" user inp))
             (fig//pumpkin-carve
              (if xs (string-to-number xs) (random 100))
              (if ys (string-to-number ys) (random 100))
              emote
              (lambda ()
                (fig//reload-pumpkin))))))
   (cons "torture bald"
         (lambda (_user msg)
           (fig//baldur-cmd (s-trim msg))))
   (cons "activate faction lighting"
         (lambda (user _)
           (when-let ((faction (format "%s" (fig//get-chatter-faction user))))
             (fig//write-chat-event (s-concat user " changed faction lighting: " faction))
             (fig//bullfrog-set "faction" faction))))
   (cons "FUSION"
         (lambda (user1 user2)
           (fig//write-chat-event (s-concat user1 " fused with " user2))
           (fig//fuse-chatters user1 user2)))
   (cons "gamba"
         (lambda (user _)
           (fig//write-chat-event (s-concat user " spins the slots"))
           (fig//slots-pull-lever)))
   (cons "start bj"
         (lambda (user _)
           (fig//write-chat-event (s-concat user " starts a game of blackjack"))
           (fig//bj-start)))
   (cons "bj: hit"
         (lambda (user _)
           (fig//write-chat-event (s-concat user " hits"))
           (fig//bj-hit)))
   (cons "bj: stand"
         (lambda (user _)
           (fig//write-chat-event (s-concat user " stands"))
           (fig//bj-stand)))
   (cons "run program"
         (lambda (user prog)
           (fig//write-chat-event (s-concat user " runs program: " prog))
           (when (eq 'out-of-fuel (fig/bless-run (fig/bless prog)))
             (fig//write-chat-event (s-concat user " ran out of fuel")))))
   (cons "feed friend"
         (lambda (user inp)
           (fig//write-chat-event (s-concat user " feeds \"friend\" " inp))
           (fig//friend-feed user inp)))
   (cons "talk to friend"
         (lambda (user inp)
           (fig//write-chat-event (s-concat user " talks to \"friend\": " inp))
           (fig//friend-chat user inp)))
   (cons "BOOST"
         (lambda (user _)
           (soundboard//play-clip "yougotboostpower.ogg")
           (fig//write-chat-event (s-concat user " boosted their boost number"))
           (fig//update-db-number user :boost (lambda (x) (+ x 1)))
           (fig//update-chat-boost-tally)))
   (cons "TSOOB"
         (lambda (user _)
           (soundboard//play-clip "rewoptsoobtoguoy.ogg" 140)
           (fig//write-chat-event (s-reverse (s-concat user " boosted their boost number")))
           (fig//update-db-number user :boost (lambda (x) (- x 1)))
           (fig//update-chat-boost-tally)))
   (cons "lend me strength"
         (lambda (user _)
           (fig//write-chat-event (s-concat user " lends me their strength"))
           (cl-incf fig//current-strength)
           (fig//render-strength)))
   (cons "MODCLONK LAUGH"
         (lambda (user _)
           (fig//write-chat-event "MODCLONK LAUGH DOT OGG")
           (when (-contains? '("LCOLONQ" "MODCLONK") user)
             (soundboard//play-clip "seinfeld.ogg"))))
   (cons "pursue idol dream"
         (lambda (user _)
           (fig//write-chat-event (format "Helping %s pursue their idol dream~" user))
           (fig//model-region-user-avatar "hair" user)))
   (cons "bells of bezelea"
         (lambda (user msg)
           (when (< (length msg) 256)
             (fig//write-chat-event (format "%s played the bells (sponsored by Bezelea)" user))
             (muzak-play-notes msg))))
   (cons "switch faction: nate"
         (lambda (user _)
           (fig//write-chat-event (format "%s switched faction to: nate" user))
           (fig//set-chatter-faction user 'nate)))
   (cons "switch faction: tony"
         (lambda (user _)
           (fig//write-chat-event (format "%s switched faction to: tony" user))
           (fig//set-chatter-faction user 'tony)))
   (cons "switch faction: lever"
         (lambda (user _)
           (fig//write-chat-event (format "%s switched faction to: lever" user))
           (fig//set-chatter-faction user 'lever)))
   (cons "lurker check in" (lambda (user _) (fig//write-chat-event (format "%s is lurking" user))))
   (cons "allow streamer to drink" (lambda (_ _) (fig//write-chat-event "drink water dummy")))
   (cons "deslug" (lambda (_ _) (fig//write-chat-event "unfold your spine")))
   (cons "spinne"
         (lambda (user _)
           (fig//write-chat-event (s-concat user " activates the spinne cyclle"))
           (fig//model-toggle "spin")))
   (cons "reverse spinne polarity"
         (lambda (user _)
           (fig//write-chat-event (s-concat user " reverses the polarity"))
           (soundboard//play-clip "reversepolarity.mp3" 75)
           (fig//model-toggle "reverse")))
   (cons "forsen"
         (lambda (user _)
           (soundboard//play-clip "cave3.ogg" 75)
           (fig/forsen)))
   (cons "Live LCOLONQ Reaction"
         (lambda (user _)
           (fig//write-chat-event (s-concat user " wants to see a live reaction"))
           (fig/live-reaction)))
   (cons "Live friend Reaction"
         (lambda (user _)
           (fig//write-chat-event (s-concat user " wants to see a live reaction (from friend)"))
           (fig/live-friend-reaction)))
   (cons "gamer"
         (lambda (user _)
           (fig//write-chat-event (s-concat user " quickscoped me"))
           (soundboard//play-clip "videogame.ogg")
           (fig/thug-life)))
   (cons "INTJ stare"
         (lambda (user _)
           (fig//write-chat-event (s-concat user " stared INTJly"))
           (fig/intj-stare)))
   (cons "arrow"
         (lambda (user msg)
           (fig//write-chat-event (format "%s points and says %S" user msg))
           (fig/clickbait msg)))
   (cons "super idol"
         (lambda (_ _)
           (fig//twitch-say "SuperIdoldexiaorongdoumeinidetianbayuezhengwudeyangguangdoumeiniyaoyanreai105Cdenididiqingchundezhen")
           (soundboard//play-clip "superidol.mp3")))
   (cons "SEASICKNESS GENERATOR" (lambda (_ _) (fig//model-toggle "zoom_wave")))
   (cons "change the letters"
         (lambda (user inp)
           (fig//write-chat-event (s-concat user " changes the letters: " inp))
           (fig//model-background-text (s-replace " " "" inp))))
   ;; (cons "use palette preset"
   ;;       (lambda (user inp)
   ;;         (fig//write-chat-event (s-concat user " changes the palette preset: " inp))
   ;;         (fig//model-use-palette-preset (s-trim inp))))
   ; (cons "create palette preset"
   ;       (lambda (user inp)
   ;         (when-let*
   ;             ((split (s-split " " inp))
   ;              (name (car split))
   ;              (hair (cadr split))
   ;              (eyes (caddr split))
   ;              (highlight (cadddr split)))
   ;           (when (-every? #'color-defined-p (list hair eyes highlight))
   ;             (fig//write-chat-event (s-concat user " creates a palette preset: " inp))
   ;             (fig//add-palette name hair eyes highlight)
   ;             (fig//model-use-palette-preset name)))))
   (cons "palette swap (hair)" (fig//handle-redeem-region-swap "hair"))
   (cons "palette swap (highlight)" (fig//handle-redeem-region-swap "highlight"))
   (cons "palette swap (eyes)" (fig//handle-redeem-region-swap "eyes"))
   ;; (cons "breed"
   ;;       (lambda (user inp)
   ;;         (fig//write-chat-event (s-concat user " asks to breed: " inp))
   ;;         (when-let* ((split (s-split " " inp))
   ;;                     (n1 (car split))
   ;;                     (n2 (cadr split))
   ;;                     (p1 (fig//get-palette n1))
   ;;                     (p2 (fig//get-palette n2)))
   ;;           (fig//breed-palettes
   ;;            p1 p2
   ;;            (lambda (p3)
   ;;              (fig//twitch-say
   ;;               (format
   ;;                "%s successfully bred %s and %s, producing %s"
   ;;                user n1 n2 (fig//palette-name p3)))
   ;;              (add-to-list 'fig/palettes p3)
   ;;              (fig//save-palettes)
   ;;              (fig//model-use-palette p3))))))
   (cons "ask computer question"
         (lambda (user inp)
           (fig//write-chat-event (s-concat user " asks the computer: " inp))
           (fig/ask inp #'fig/say)))
   (cons "say thing"
         (lambda (user inp)
           (fig//write-chat-event (s-concat user " sends TTS: " inp))
           (fig/say (s-trim inp))))
   (cons "haunt unit"
         (lambda (user inp)
           (fig//write-chat-event (format "%s is now haunting %s" user inp))
           (fe8//associate-user-character user inp)))
   (cons "VIPPER"
         (lambda (user _)
           (fig//write-chat-event (s-concat user " VIPed themself"))
           (when (>= (length fig//twitch-vip-list) 49)
             (fig//remove-random-vip))
           (fig//add-vip user)
           (fig//twitch-get-vip-list)))
   (cons "crown a king and/or queen"
         (lambda (user inp)
           (soundboard//play-clip "girlfriend.ogg")
           (fig//write-chat-event (s-concat user " VIPed " inp))
           (when (>= (length fig//twitch-vip-list) 49)
             (fig//remove-random-vip))
           (fig//add-vip (string-remove-prefix "@" inp))))
   (cons "deVIPPER"
         (lambda (user inp)
           (fig//write-chat-event (s-concat user " removed VIP from " inp))
           (fig//remove-vip (string-remove-prefix "@" inp))))
  ))

(defvar fig//chat-header-line "")
(defvar-keymap fig/chat-mode-map
  :suppress t
  "C-l" #'fig//clear-chat
  )
(define-derived-mode fig/chat-mode special-mode "Twitch Chat"
  "Major mode for displaying Twitch chat."
  :group 'fig
  (setq-local window-point-insertion-type t)
  (setq-local header-line-format '(:eval fig//chat-header-line)))

(defun fig//shuffle-seq (s)
  "Shuffle S."
  (if (seq-empty-p s)
      nil
    (let ((elt (seq-elt s (random (seq-length s)))))
      (cons elt (fig//shuffle-seq (remove elt s))))))

(defun fig//write (text &optional face)
  "Write TEXT to the current buffer and apply FACE."
  (let ((text-final (if face (propertize text 'face face) text)))
    (insert text-final)))

(defun fig//write-line (line &optional face)
  "Write LINE and a newline to the current buffer and apply FACE."
  (fig//write (concat line "\n") face))

(defun fig//decode-string (s)
  "Decode the base64 UTF-8 string S."
  (decode-coding-string (base64-decode-string s) 'utf-8))

(defun fig//encode-string (s)
  "Decode the base64 UTF-8 string S."
  (base64-encode-string (encode-coding-string s 'utf-8) t))

(defun fig//clean-string (s)
  "Remove special characters from S."
  ;; (replace-regexp-in-string "[^[:ascii:]^[:print:]]" "" s)
  (replace-regexp-in-string "[^[:print:]]" "" s)
  )

(defun fig//write-log (line &optional face)
  "Write LINE to the log buffer and apply FACE."
  (with-current-buffer (get-buffer-create fig/log-buffer)
    (goto-char (point-max))
    (fig//write-line (fig//clean-string (format "%s" line)) face)
    (goto-char (point-max))))

(defun fig//get-twitch-chat-buffer ()
  "Return the Twitch chat buffer."
  (unless (get-buffer fig/twitch-chat-buffer)
    (with-current-buffer (get-buffer-create fig/twitch-chat-buffer)
      (fig/chat-mode)))
  (get-buffer fig/twitch-chat-buffer))

(defun fig//add-vip (user)
  "Give VIP status to USER."
  (fig/pub '(monitor twitch vip add) (list user)))

(defun fig//remove-vip (user)
  "Remove VIP status from USER."
  (fig/pub '(monitor twitch vip remove) (list user)))

(defun fig//remove-random-vip ()
  "Remove VIP status from a random user."
  (let ((user (nth (random (length fig//twitch-vip-list)) fig//twitch-vip-list)))
    (fig//write-chat-event (format "Removing VIP randomly from: %s" user))
    (fig/pub '(monitor twitch vip remove) (list user))))

(defun fig//create-poll (title options &optional callback)
  "Create a poll with TITLE and OPTIONS.
CALLBACK will be passed the winner when the poll concludes."
  (unless fig//current-poll-callback
    (setq fig//current-poll-callback callback)
    (fig/pub
     '(monitor twitch poll create)
     (list (s-truncate 60 (s-trim title)) options))))

(defun fig//create-prediction (title options)
  "Create a prediction with TITLE and OPTIONS."
  (unless fig//current-prediction-ids
    (fig/pub '(monitor twitch prediction create) (list title options))))

(defun fig//finish-prediction (outcome)
  "Finish the current prediction with winning OUTCOME."
  (when fig//current-prediction-ids
    (fig/pub
     '(monitor twitch prediction finish)
     (list (car fig//current-prediction-ids)
           (car (alist-get outcome (cadr fig//current-prediction-ids) nil nil #'s-equals?))))))

(defun fig//twitch-say (msg)
  "Write MSG to Twitch chat."
  (let ((trimmed (s-trim msg)))
    (fig//write-chat-message "LCOLONQ" "866686220" trimmed "#616161")
    (fig/pub '(monitor twitch chat outgoing) (list trimmed))))

(defun fig//discord-say (nm msg)
  "Write MSG to Discord chat as NM."
  (let ((trimmed (s-trim msg)))
    (fig/pub
     '(monitor discord chat outgoing)
     (list
      (base64-encode-string nm t)
      (base64-encode-string trimmed t)))))

(defun fig//write-chat-event (ev)
  "Write EV to the Twitch chat buffer."
  (let ((inhibit-read-only t))
    (with-current-buffer (fig//get-twitch-chat-buffer)
      (goto-char (point-max))
      (insert (propertize ev 'face 'italic))
      (insert "\n"))))

(defun fig//user-sigil (user &optional badges)
  "Return the sigil character for USER with BADGES."
  (let ((equity (fig//load-db-entry user :equity)))
    (cond
     ((-contains? badges "broadcaster/1") "(it me)")
     ((-contains? badges "moderator/1") "⚔")
     ((and equity (> equity 0)) "♿")
     ((-contains? badges "vip/1") "💎")
     ((-contains? badges "subscriber/0") "💻")
     )))

(defun fig//chat-button-action (b)
  "Action run on button press for button B."
  (let ((userid (get-text-property (button-start b) 'fig-user-id)))
    (fig//semi-src userid)))
(defun fig//chat-bible-button-action (b)
  "Action run on button press for button B."
  (let ((text (get-text-property (button-start b) 'fig-message)))
    (fig/biblical text (lambda (ass) (fig/say (s-concat "Spiritual assessment: " ass))))))
(defun fig//write-chat-message (user userid text &optional color sigil bible-score buf)
  "Write TEXT to the Twitch chat buffer as USER with USERID and COLOR."
  (let ((inhibit-read-only t))
    (with-current-buffer (or buf (fig//get-twitch-chat-buffer))
      (goto-char (point-max))
      (insert-text-button
       (s-concat (if sigil (s-concat sigil " ") "") user)
       'face (list :foreground color :weight 'bold)
       'fig-user-id userid
       'action #'fig//chat-button-action)
      (insert
       (propertize
        ": "
        'face
        (list
         :foreground
         (cl-case (fig//get-chatter-faction user)
           (nate "pink")
           (lever "lightblue")
           (tony "lightgreen")
           (t "white"))
         )
        ))
      (insert text)
      (when bible-score
        (let* ((wwidth (- (window-total-width (get-buffer-window (current-buffer))) 3))
               (bible-button-text (format "[biblicality %.2f]" bible-score))
               ;; (bible-button-text "[biblicality -666]")
               (msgwidth
                (+ (length sigil) (if sigil 1 0)
                   (length user) (length ": ") (length text)
                   (length bible-button-text)))
               (lines (+ 1 (/ msgwidth wwidth))))
          (insert
           (propertize
            " " 'display
            `(space
              :align-to
              ,(- (+ (* wwidth lines) (- lines 1))
                  (length bible-button-text)
                  ))))
          (insert-text-button
           bible-button-text
           'face '(:foreground "#bbbbbb")
           'fig-message text
           'action #'fig//chat-bible-button-action)))
      (insert "\n"))))

(defconst fig//godot-logo
  (propertize
   "godot"
   'display
   (create-image "/home/llll/src/fig/misc/godot.png")
   'rear-nonsticky t))

(defconst fig//powershell-logo
  (propertize
   "powershell"
   'display
   (create-image "/home/llll/src/fig/misc/powershell_small.png")
   'rear-nonsticky t))

(defun fig//handle-twitch-message (msg)
  "Write MSG to the chat buffer, processing any commands."
  (fig//write-log (format "%s" msg))
  (let* ((user (fig//decode-string (car msg)))
         (tags (cadr msg))
         (userid (car (alist-get "user-id" tags nil nil #'s-equals?)))
         (color (car (alist-get "color" tags nil nil #'s-equals?)))
         (emotes (car (alist-get "emotes" tags nil nil #'s-equals?)))
         (badges (s-split "," (car (alist-get "badges" tags nil nil #'s-equals?))))
         (text (fig//decode-string (caddr msg)))
         (text-colored-bible-res (fig//bible-colorize-sentence text))
         (text-colored-bible (car text-colored-bible-res))
         (text-with-emotes
          (s-replace
           "[i](this was sent from godot)[/i]"
           fig//godot-logo
           (fig//add-7tv-emotes
            (fig//process-emote-ranges
             (s-split "/" emotes)
             (if fig//assess-chat-spirituality text-colored-bible text)))))
         )
    (fig//assign-chatter-faction user)
    (fig//check-chatter-geiser user)
    (fig//hexamedia-update-user user)
    (push (cons user text) fig//incoming-chat-history)
    (setf (alist-get user fig//chatter-colors nil nil #'s-equals?) color)
    (when (s-equals? user "MODCLONK")
      (fig//obs-log-modclonk-message))
    (fig//write-chat-message
     user userid (s-replace "bald" "ball" text-with-emotes) color
     (fig//user-sigil user badges)
     (and fig//assess-chat-spirituality (cdr text-colored-bible-res)))
    
    (--each fig//twitch-chat-commands
      (when (s-contains? (car it) text)
        (funcall (cdr it) user text)))))

(defun fig//handle-discord-message (msg)
  "Write MSG to the chat buffer, processing any commands."
  (let* ((user (fig//decode-string (car msg)))
         (text (fig//decode-string (caddr msg)))
         (text-colored-bible-res (fig//bible-colorize-sentence text))
         (text-colored-bible (car text-colored-bible-res))
         (text-with-emotes
          (s-replace
           "*(this was sent from PowerShell)*"
           fig//powershell-logo
           (fig//add-7tv-emotes
            (if fig//assess-chat-spirituality text-colored-bible text)))))
    (fig//write-chat-message
     user "none" (s-replace "bald" "ball" text-with-emotes) nil
     (fig//user-sigil user nil)
     (and fig//assess-chat-spirituality (cdr text-colored-bible-res)))))

(defun fig//handle-redeem (r)
  "Handle the channel point redeem R."
  (fig//write-log r)
  (let* ((user (car r))
         (redeem (cadr r))
         (encoded-input (caddr r))
         (input (when encoded-input (fig//decode-string encoded-input)))
         (handler (alist-get redeem fig//twitch-redeems nil nil #'s-equals?)))
    (if handler
        (funcall handler user input)
      (fig//write-log (format "Unknown channel point redeem: %S" redeem)))))

(defun fig//clear-chat ()
  "Clear the Twitch chat buffer."
  (interactive)
  (with-current-buffer (fig//get-twitch-chat-buffer)
    (let ((inhibit-read-only t))
      (erase-buffer))))

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

(provide 'fig)
;;; fig.el ends here
