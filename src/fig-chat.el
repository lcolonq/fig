;;; fig-chat --- Chat display -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'dash)
(require 's)
(require 'evil)

(defcustom fig/twitch-chat-buffer "*fig-chat*"
  "Name of buffer used to store the chat log."
  :type '(string)
  :group 'fig)

(defun fig//get-twitch-chat-buffer (&optional nm)
  "Return the Twitch chat buffer.
Optionally, return the buffer NM in Twitch chat mode."
  (let ((bufnm (or nm fig/twitch-chat-buffer)))
    (unless (get-buffer bufnm)
      (with-current-buffer (get-buffer-create bufnm)
        (fig/chat-mode)))
    (get-buffer bufnm)))

(defvar fig//chat-header-line "")

(defvar-keymap fig/chat-mode-map
  :suppress t
  "C-l" #'fig//clear-chat
  ;; "<mouse-movement>" #'fig/chat-handle-mouse
  )
(evil-define-key 'motion fig/chat-mode-map (kbd "<return>") #'fig/open-link)

(defun fig//clear-chat ()
  "Clear the Twitch chat buffer."
  (interactive)
  (with-current-buffer (fig//get-twitch-chat-buffer)
    (let ((inhibit-read-only t))
      (erase-buffer))))

(defun fig/open-link ()
  "Open URL in the primary stream window."
  (interactive)
  (when-let ((url (thing-at-point 'url t)))
    (select-window (colonq/get-stream-primary-window))
    (browse-url url)))

(defun fig//prevent-focus-frame (e)
  "Prevent focus from reaching popup frame E."
  (not (frame-parameter (cadr e) 'fig-prevent-focus)))

(define-derived-mode fig/chat-mode special-mode "Twitch Chat"
  "Major mode for displaying Twitch chat."
  :group 'fig
  (add-hook 'post-command-hook #'fig//handle-clonkhead-io nil t)
  (advice-add 'handle-switch-frame :before-while #'fig//prevent-focus-frame)
  ;; (setq-local track-mouse t)
  (setq-local window-point-insertion-type t)
  (cond
   (t (setq-local header-line-format '(:eval fig//chat-header-line)))))

(defun fig/chat-handle-mouse (event)
  "Handle a mouse movement EVENT in Twitch chat."
  (interactive "e")
  (when-let*
      ((e (cadr event))
       (point (posn-point e))
       (same-win (eq (selected-window) (posn-window e))))
    (with-current-buffer (fig//get-twitch-chat-buffer)
      (fig//update-clonkhead-io
       (get-text-property point 'fig-user)
       (window-absolute-pixel-position point)))))

(defun fig//write-chat-event (ev)
  "Write EV to the Twitch chat buffer."
  (let ((inhibit-read-only t))
    (with-current-buffer (fig//get-twitch-chat-buffer)
      (goto-char (point-max))
      (insert (propertize ev 'face 'italic))
      (insert "\n"))))

(defun fig//user-sigil (user &optional badges)
  "Return the sigil character for USER with BADGES."
  (let ((equity (fig//load-db-entry user :equity))
        ;; (aoc-stars (fig//lookup-aoc-stars user))
        ;; (max-stars (fig//max-aoc-stars)))
        )
    (cond
     ;; ((and aoc-stars (>= aoc-stars max-stars)) "🎄")
     ;; (aoc-stars "🌲")
     ((-contains? badges "broadcaster/1") "(it me)")
     ((-contains? badges "moderator/1") "⚔")
     ((-contains? badges "artist-badge/1") "🖌️")
     ((and equity (> equity 0))
      (cond
       ((s-equals? user "Bezelea") "♿🔔")
       ((s-equals? user "AltoVT") "📈")
       (t "EL.")))
     ((-contains? badges "vip/1") "💎")
     ((-contains? badges "subscriber/0") "💻")
     )))

(defun fig//chat-button-action (b)
  "Action run on button press for button B."
  (let ((user (get-text-property (button-start b) 'fig-user))
        (pos (window-absolute-pixel-position (button-start b))))
    ;; (fig//semi-src userid)
    (fig//display-clonkhead-io user (car pos) (cdr pos))
    ))
(defun fig//chat-bible-button-action (b)
  "Action run on button press for button B."
  (let ((text (get-text-property (button-start b) 'fig-message)))
    (fig/biblical text (lambda (ass) (fig/say (s-concat "Spiritual assessment: " ass))))))
(defun fig//write-chat-message (user userid text &optional color sigil bible-score buf)
  "Write TEXT to the Twitch chat buffer as USER with USERID and COLOR."
  (let ((inhibit-read-only t)
        (name
         (cond
          (fig//identity-chat-toggle (fig//get-chatter-identity user))
          ;; ((s-equals? "fn_lumi" user) "Lumifer")
          (t user))))
    (with-current-buffer (or buf (fig//get-twitch-chat-buffer))
      (goto-char (point-max))
      (insert-text-button
       (s-concat (if sigil (s-concat sigil " ") "") name)
       'face (list :foreground color :weight 'bold)
       'fig-user user
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
               ;; (bible-button-text (format "[L:Quality %.2f]" bible-score))
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
   (create-image "/home/llll/src/fig/assets/misc/godot.png")
   'rear-nonsticky t))

(defconst fig//powershell-logo
  (propertize
   "powershell"
   'display
   (create-image "/home/llll/src/fig/assets/misc/powershell_small.png")
   'rear-nonsticky t))

(defconst fig//chat-commands
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
             (s-join " " (--filter (s-contains? "!" it) (-map #'car fig//chat-commands)))))))
   (cons "!today"
         (lambda (_ _)
           (fig//twitch-say
            (s-trim
             (with-temp-buffer
               (insert-file-contents-literally "~/today.txt")
               (buffer-string))))))
   (cons "!nc" (lambda (_ _) (fig//twitch-say "try: \"nc colonq.computer 31340\", if nc doesn't work try ncat or telnet")))
   (cons "!oomfie" (lambda (_ _) (fig//twitch-say "hi!!!!!!!")))
   (cons "!pronunciation" (lambda (_ _) (fig//twitch-say (fig//pronuciation))))
   ;; (cons "!jetsWave" (lambda (_ _) (fig//twitch-say (fig/slurp "jetsWave.txt"))))
   (cons "!forth" (lambda (_ _) (fig//twitch-say "https://github.com/lcolonq/giving")))
   (cons "!oub" (lambda (_ _) (fig//twitch-say "https://oub.colonq.computer")))
   (cons "!game" (lambda (_ _) (fig//twitch-say "https://oub.colonq.computer")))
   (cons "!pubnix" (lambda (_ _) (fig//twitch-say "https://pub.colonq.computer")))
   (cons "!ring" (lambda (_ _) (fig//twitch-say "https://pub.colonq.computer")))
   (cons "!webring" (lambda (_ _) (fig//twitch-say "https://pub.colonq.computer")))
   (cons "!animeguide" (lambda (_ _) (fig//twitch-say "https://nixos-and-flakes.thiscute.world/introduction")))
   (cons "!bells" (lambda (_ _) (fig//twitch-say "https://pub.colonq.computer/~bezelea/bells/ and https://pub.colonq.computer/~prod/toy/dbkai/")))
   (cons "!help" (lambda (_ _) (fig//twitch-say "https://pub.colonq.computer/~prod/toy/glossary/")))
   (cons "!faction"
         (lambda (user _)
           (fig//twitch-say (format "faction for %s: %s" user (fig//get-chatter-faction user)))))
   (cons "!thanks" (lambda (user _) (fig//twitch-say (format "thank you %s!" user))))
   (cons "!namesake"
         (lambda (user _)
           (fig//twitch-say (s-replace "\n" ", " (s-trim (fig//describe-ancestor (fig//get-chatter-ancestor user)))))))
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
   (cons "!8ball"
         (lambda (user inp)
           (let ((trimmed (s-trim (s-replace "!8ball" "" inp))))
             (fig//8ball
              trimmed
              (lambda (answer)
                (fig//twitch-say (format "@%s 8ball says: %s" user answer)))))))
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
   (cons "!resolution"
         (lambda (user inp)
           (let ((trimmed (s-trim (s-replace "!resolution" "" inp))))
             (if (string-empty-p trimmed)
                 (fig//write-chat-event "You gotta put what your resolution is.")
               (fig//write-chat-event (format "%s RESOLVES: %s" (s-upcase user) trimmed))
               (fig//set-db-entry user :resolution trimmed)))))
   (cons "!twitter"
         (lambda (_ _)
           (fig/ask "How do you feel about Twitter? Should viewers follow LCOLONQ on Twitter?" #'fig/say)
           (fig//twitch-say "https://twitter.com/LCOLONQ")))
   (cons "heart" (lambda (_ _) (fig/increment-heartrate-counter)))
   (cons "bpm" (lambda (_ _) (fig/increment-heartrate-counter)))
   (cons "BPM" (lambda (_ _) (fig/increment-heartrate-counter)))
   (cons "discord" (lambda (_ _) (fig//twitch-say "https://discord.gg/f4JTbgN7St")))
   (cons "Discord" (lambda (_ _) (fig//twitch-say "https://discord.gg/f4JTbgN7St")))
   (cons "!irc" (lambda (_ _) (fig//twitch-say "#cyberspace on IRC at colonq.computer:26697 (over TLS)")))
   (cons "IRC" (lambda (_ _) (fig//twitch-say "#cyberspace on IRC at colonq.computer:26697 (over TLS)")))
   ;; (cons "!aoc" (lambda (_ _) (fig//twitch-say "Join our leaderboard: 3307583-b61f237c")))
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
   (cons "!fish"
         (lambda (_ _)
           (fig//twitch-say (shell-command-to-string "fishing"))))
   ;; (cons "!roll" (lambda (user _) (fig//twitch-say (fig//character-to-string (fig//roll-character user)))))
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
   ;; (cons
   ;;  "!vippers"
   ;;  (lambda (_ _)
   ;;    (let ((vipperstring (s-join ", " (fig//shuffle-seq fig//twitch-vip-list))))
   ;;      (fig//twitch-say (seq-take vipperstring 450)))
   ;;    (fig//twitch-get-vip-list)))
   ;; (cons "!levelup"
   ;;       (lambda (user _)
   ;;         (fig//update-db-character
   ;;          user
   ;;          (lambda (c)
   ;;            (cl-incf (fig//rpg-character-level c))
   ;;            c))
   ;;         (fig//twitch-say (fig//character-to-string (fig//get-db-character user)))))
   ))

(provide 'fig-chat)
;;; fig-chat.el ends here
