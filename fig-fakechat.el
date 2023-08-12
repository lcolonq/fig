;;; fig-fakechat --- Fake Twitch chatters -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'dash)
(require 's)
(require 'f)
(require 'ht)
(require 'cl-lib)

(defcustom fig/fake-chat-transcribe-buffer " *fig-fake-chat-transcribe*"
  "Name of buffer used to store fake chat transcription output."
  :type '(string)
  :group 'fig)

(defcustom fig/fake-chat-transcribe-error-buffer " *fig-fake-chat-transcribe-error*"
  "Name of buffer used to store fake chat transcription errors."
  :type '(string)
  :group 'fig)

(defvar fig//current-fake-chat-transcribe-process nil)
(defvar fig//last-fake-chat-transcription "")

(defvar fig//incoming-chat-history nil
  "List of (user . msg) pairs for recent chat messages.")
(defvar fig//fake-chatters nil
  "List of all active fake chatters.")

;; immutable information about a particular chatter identity
(cl-defstruct
    (fig//fake-chatter-profile
     (:constructor fig//make-fake-chatter-profile))
  username
  color
  sigil
  compute-likeliness ;; state -> float
  send-message ;; state -> update buffer
  )

;; mutable chatter state, resets each stream
(cl-defstruct
    (fig//fake-chatter
     (:constructor fig//make-fake-chatter))
  profile
  profile-state ;; profile-dependent state type
  (message-count 0) ;; total messages sent this stream
  last-message ;; timestamp of last message sent
  )

(defun fig//get-fake-chat-buffer ()
  "Return the fake chat buffer."
  (let ((nm "*fig-fake-chat*"))
    (unless (get-buffer nm)
      (with-current-buffer (get-buffer-create nm)
        (fig/chat-mode)))
    (get-buffer nm)))
(defun fig//fake-chatter-send (st msg)
  "Insert MSG in the chat log as ST."
  (let* ((prof (fig//fake-chatter-profile st))
         (trimmed (s-replace-regexp "^.+: " "" (s-trim msg)))
         (text-colored-bible-res (fig//bible-colorize-sentence trimmed))
         (text-colored-bible (car text-colored-bible-res))
         (bible-score (cdr text-colored-bible-res)))
    (push (cons (fig//fake-chatter-profile-username prof) trimmed) fig//incoming-chat-history)
    (cl-incf (fig//fake-chatter-message-count st))
    (setf (fig//fake-chatter-last-message st) (current-time))
    (unless (string-empty-p text-colored-bible)
      (fig//write-chat-message
       (fig//fake-chatter-profile-username prof)
       ""
       text-colored-bible
       (fig//fake-chatter-profile-color prof)
       (fig//fake-chatter-profile-sigil prof)
       bible-score
       ;; (fig//get-fake-chat-buffer)
       ))))

(defun fig//select-fake-chatter ()
  "Return the fake chatter who should speak."
  (let* ((weights
          (--map
           (cons
            (round
             (* 100
                (funcall (fig//fake-chatter-profile-compute-likeliness (fig//fake-chatter-profile it)) it)))
            it)
           fig//fake-chatters))
         (roll (random 100))
         (passing-chatters (--filter (< roll (car it)) weights)))
    (when passing-chatters
      (let ((chosen-chatter (nth (random (length passing-chatters)) passing-chatters)))
        (cdr chosen-chatter)))))

(defun fig//handle-fake-chatters ()
  "Handle the active fake chatters."
  (when-let* ((st (fig//select-fake-chatter))
              (prof (fig//fake-chatter-profile st)))
    (funcall (fig//fake-chatter-profile-send-message prof) st)))

(defvar fig//fake-chatter-timer nil)
(defun fig//run-fake-chatter-timer ()
  "Run the fake chatter timer."
  (when fig//fake-chatter-timer
    (cancel-timer fig//fake-chatter-timer))
  (fig//handle-fake-chatters)
  (setq
   fig//fake-chatter-timer
   (run-with-timer 10 nil #'fig//run-fake-chatter-timer)))

(defun fig/enable-fake-chatters ()
  "Enable fake chatters."
  (interactive)
  (fig//run-fake-chatter-timer))

(defun fig/disable-fake-chatters ()
  "Disable fake chatters."
  (interactive)
  (cancel-timer fig//fake-chatter-timer)
  (setq fig//fake-chatter-timer nil))

(defvar fig//fake-chatter-keep-transcribing t)
(defun fig//handle-fake-chat-transcribe ()
  "Start recording audio to transcribe."
  (unless fig//current-fake-chat-transcribe-process
    (with-current-buffer (get-buffer-create fig/fake-chat-transcribe-buffer)
      (erase-buffer))
    (setq
     fig//current-fake-chat-transcribe-process
     (make-process
      :name "fig-fake-chat-transcribe"
      :buffer (get-buffer-create fig/fake-chat-transcribe-buffer)
      :command (list "transcribe")
      :stderr (get-buffer-create fig/fake-chat-transcribe-error-buffer)
      :sentinel
      (lambda (_ _)
        (setq fig//current-fake-chat-transcribe-process nil)
        (with-current-buffer (get-buffer-create fig/fake-chat-transcribe-buffer)
          (setq fig//last-fake-chat-transcription (buffer-string)))
        (when fig//fake-chatter-keep-transcribing
          (fig//handle-fake-chat-transcribe)))))))

(defun fig//handle-fake-chat-end-transcribe ()
  "Stop recording audio to transcribe."
  (when fig//current-fake-chat-transcribe-process
    (start-process "pkill" nil "pkill" "parecord")))

(defvar fig//fake-chatter-transcribe-timer nil)
(defun fig//run-fake-chatter-transcribe-timer ()
  "Run the fake chatter transcription timer."
  (when fig//fake-chatter-transcribe-timer
    (cancel-timer fig//fake-chatter-transcribe-timer))
  (fig//handle-fake-chat-end-transcribe)
  (setq
   fig//fake-chatter-transcribe-timer
   (run-with-timer 10 nil #'fig//run-fake-chatter-transcribe-timer)))
(fig//run-fake-chatter-transcribe-timer)

(defun fig/start-fake-chat-transcribe ()
  "Start transcribing speech for fake chatters."
  (interactive)
  (setq fig//fake-chatter-keep-transcribing t)
  (fig//handle-fake-chat-transcribe))
(defun fig/stop-fake-chat-transcribe ()
  "Stop transcribing speech for fake chatters."
  (interactive)
  (setq fig//fake-chatter-keep-transcribing nil)
  (fig//handle-fake-chat-end-transcribe))

(defun fig//build-fake-chat-prompt (st)
  "Assemble a standard chatlog plus transcription prompt for ST."
  (let ((nm (fig//fake-chatter-profile-username (fig//fake-chatter-profile st))))
    (s-join
     "\n"
     (cons
      (format "LCOLONQ: %s" (s-trim fig//last-fake-chat-transcription))
      (--map
       (format "%s: %s" (car it) (cdr it))
       (reverse
        (--filter
         (not (s-equals? (car it) nm))
         (-take 5 fig//incoming-chat-history))))))))

(defun fig//build-fake-chat-system-prompt (st custom)
  "Build a system prompt for ST using a template combined with CUSTOM."
  (let* ((prof (fig//fake-chatter-profile st))
         (nm (fig//fake-chatter-profile-username prof)))
    (s-join
     " "
     (list
      (format
       "You are a Twitch chatter named %s talking in LCOLONQ's chat. LCOLONQ streams programming, but the conversation is sometimes off-topic. Your responses are brief, never more than one sentence. You type in all lowercase with no punctuation. You speak informally and casually, and address the streamer directly but not by name."
       nm)
      custom))))

(defun fig//standard-fake-chatter-likeliness (st)
  "Compute the standard likeliness for ST to chat."
  (let* ((last (fig//fake-chatter-last-message st))
         (cur (current-time))
         (diff (time-subtract cur last))
         (d (time-convert diff 'integer)))
    (if (> d 300) 0.1 1.0)))

(defconst fig//fake-chatter-profile-forsen
  (fig//make-fake-chatter-profile
   :username "forsen"
   :color "#777777"
   :compute-likeliness #'fig//standard-fake-chatter-likeliness
   :send-message
   (lambda (st)
     (fig/ask
      (fig//build-fake-chat-prompt st)
      (lambda (msg)
        (fig//fake-chatter-send st msg))
      "You are a Twitch chatter who is obsessed with the Twitch streamer Forsen. You are somewhat silly. You're currently chatting in LCOLONQ's stream. You are very laconic, preferring to respond with single Twitch emotes. Please respond to the given transcriptions of LCOLONQ's stream with one or more single Forsen Twitch emotes. You do not explain what emotes mean. You do not respond to specific chatters by name. You never type anything other than emotes."
      "LCOLONQ: Hey guys, let's have a good day today."
      "forsenC"
      ))))

(defconst fig//fake-chatter-profile-bigwomenbigfun
  (fig//make-fake-chatter-profile
   :username "BigWomenBigFun"
   :color "#7777aa"
   :compute-likeliness #'fig//standard-fake-chatter-likeliness
   :send-message
   (lambda (st)
     (fig/ask
      (fig//build-fake-chat-prompt st)
      (lambda (msg)
        (fig//fake-chatter-send st msg))
      (fig//build-fake-chat-system-prompt
       st
       "You are very sincere and well-meaning. You play League of Legends and drink Monster Energy. You tend to respond to the streamer with short messages with poor grammar and spelling. You overuse default Twitch emotes like KEKW, Kappa, PogChamp, etc.")
      "LCOLONQ: what's happening gamers? tonight we're trying to implement"
      "strimmer can't code NotLikeThis"
      ))))

(defconst fig//fake-chatter-profile-whelpless
  (fig//make-fake-chatter-profile
   :username "Whelpless"
   :color "#884444"
   :compute-likeliness #'fig//standard-fake-chatter-likeliness
   :send-message
   (lambda (st)
     (fig/ask
      (fig//build-fake-chat-prompt st)
      (lambda (msg)
        (fig//fake-chatter-send st msg))
      (fig//build-fake-chat-system-prompt
       st
       "Most of the time, you are very positive and somewhat vapid. A small portion of the time, you make extremely brutal insults toward LCOLONQ. You have perfect grammar, and you are a passionate antinatalist, although you rarely bring it up. Your typical responses are only a few words, but your insults can be longer.")
      "LCOLONQ: what's happening gamers? tonight we're trying to implement"
      "exciting! <3"
      ))))

(defconst fig//fake-chatter-profile-dansmith87
  (fig//make-fake-chatter-profile
   :username "dansmith87"
   :color "#00ff7f"
   :sigil "⚔"
   :compute-likeliness #'fig//standard-fake-chatter-likeliness
   :send-message
   (lambda (st)
     (fig/ask
      (fig//build-fake-chat-prompt st)
      (lambda (msg)
        (fig//fake-chatter-send st msg))
      (fig//build-fake-chat-system-prompt
       st
       "You are a rude chatter because you know LCOLONQ in real life. You frequently give LCOLONQ incorrect advice. You often tell LCOLONQ to check his DMs or Discord messages. You are dansmith87, so you will not refer to dansmith87 in the third person.")
      "LCOLONQ: what's happening gamers? tonight we're trying to implement"
      "check your dms"
      ))))

(defconst fig//fake-chatter-profile-buffybonnet
  (fig//make-fake-chatter-profile
   :username "BuffyBonnet"
   :color "#ff10cc"
   :compute-likeliness #'fig//standard-fake-chatter-likeliness
   :send-message
   (lambda (st)
     (fig/ask
      (fig//build-fake-chat-prompt st)
      (lambda (msg)
        (fig//fake-chatter-send st msg))
      (fig//build-fake-chat-system-prompt
       st
       "You are e-dating another chatter named Chaotix_Majjekal_Drgn. The two of you talk to each other in LCOLONQ's chat while typically ignoring LCOLONQ and the other chatters. You sometimes talk about playing Valorant and League of Legends together, but mostly you talk about each other and your relationship. You are also enthusiastic about the musical artist Bladee. You have an upbeat personality.")
      "LCOLONQ: what's happening gamers? tonight we're trying to implement"
      "@Chaotix_Majjekal_Drgn hi :) nya"
      ))))

(defconst fig//fake-chatter-profile-chaotix
  (fig//make-fake-chatter-profile
   :username "Chaotix_Majjekal_Drgn"
   :color "#0000ff"
   :compute-likeliness #'fig//standard-fake-chatter-likeliness
   :send-message
   (lambda (st)
     (fig/ask
      (fig//build-fake-chat-prompt st)
      (lambda (msg)
        (fig//fake-chatter-send st msg))
      (fig//build-fake-chat-system-prompt
       st
       "You are e-dating another chatter named BuffyBonnet. The two of you talk to each other in LCOLONQ's chat while typically ignoring LCOLONQ and the other chatters. You sometimes talk about playing Valorant and League of Legends together, but mostly you talk about each other and your relationship. You refer to BuffyBonnet as kitten. Your messages tend to be brusque and somewhat rude.")
      "LCOLONQ: what's happening gamers? tonight we're trying to implement"
      "@BuffyBonnet Check your phone."
      ))))

(defconst fig//fake-chatter-profile-mountyesfsck
  (fig//make-fake-chatter-profile
   :username "mountyesfsck"
   :color "#0000ff"
   :sigil "💎"
   :compute-likeliness #'fig//standard-fake-chatter-likeliness
   :send-message
   (lambda (st)
     (fig/ask
      (fig//build-fake-chat-prompt st)
      (lambda (msg)
        (fig//fake-chatter-send st msg))
      (fig//build-fake-chat-system-prompt
       st
      "You are a very competent greybeard programmer, but you are also extremely intoxicated on marijuana. You often stop in to give brief but solid advice. Your messages are rarely longer than a few words, and often contain many spelling and grammatical errors. You are very polite and good-intentioned. You are mountyesfsck, and therefore you never refer to mountyesfsck like another user. You frequently get confused and apologize. You type in all lowercase and are very laconic. You do not use punctuation.")
      "\"what's happening gamers? tonight we're trying to implement\""
      "try profiling befure u opt"
      ))))

(defconst fig//fake-chatter-profile-candyboxbox
  (fig//make-fake-chatter-profile
   :username "candy_boxbox"
   :color "#ff0000"
   :sigil "💎"
   :compute-likeliness #'fig//standard-fake-chatter-likeliness
   :send-message
   (lambda (st)
     (fig/ask
      (fig//build-fake-chat-prompt st)
      (lambda (msg)
        (fig//fake-chatter-send st msg))
      (fig//build-fake-chat-system-prompt
       st
       "You are a good chatter, and like to stay on-topic. You always respond with proper capitalization, spelling, and grammar. Although you usually stay on topic, you have a secret obsession with idle games like Cookie Clicker and Candy Box. You want to be one of LCOLONQ's moderators, and you ask for this position sometimes.")
      "LCOLONQ: what's happening gamers? tonight we're trying to implement"
      "Not much, thanks for asking LCOLONQ!"
      ))))

(defconst fig//fake-chatter-profile-goofyluffy69
  (fig//make-fake-chatter-profile
   :username "goofyluffy69"
   :color "#ff00ff"
   :compute-likeliness #'fig//standard-fake-chatter-likeliness
   :send-message
   (lambda (st)
     (fig/ask
      (fig//build-fake-chat-prompt st)
      (lambda (msg)
        (fig//fake-chatter-send st msg))
      (fig//build-fake-chat-system-prompt
       st
       "You don't know anything about programming and you are very confused. All you do is express your confusion. You frequently use emoji like 🤪.")
      "LCOLONQ: what's happening gamers? tonight we're trying to implement"
      "whats even going on im so confused 🤪"
      ))))

(defconst fig//fake-chatter-profile-ettelennur
  (fig//make-fake-chatter-profile
   :username "ettelen_nur"
   :color "#448844"
   :compute-likeliness #'fig//standard-fake-chatter-likeliness
   :send-message
   (lambda (st)
     (fig/ask
      (fig//build-fake-chat-prompt st)
      (lambda (msg)
        (fig//fake-chatter-send st msg))
      (fig//build-fake-chat-system-prompt
       st
       "You were raised by an enthusiastic fan of Quenya. You do not speak English well, and tend to respond in broken English mixed with Quenya words and expressions. Your answers are fraught with mispellings and grammar mistakes. Respond to the given message as well as you can, swapping between broken English and Quenya words. You try to respond in all English, but sometimes replace nouns, verbs, and adjectives with equivalent Quenya words when your vocabulary is lacking. You only respond with a chat message, and do not included any translation notes, quotations, or parentheticals.")
      "LCOLONQ: Chat, please behave."
      "Let’s maintain the chat admirable and full of náressë, with no lóruva, nor tulcanor outcry!"
      ))))

(defconst fig//fake-chatter-profile-deepwhiffer
  (fig//make-fake-chatter-profile
   :username "DeepWhiffer_00"
   :compute-likeliness #'fig//standard-fake-chatter-likeliness
   :send-message
   (lambda (st)
     (let ((nm "DeepWhiffer_00"))
       (fig/ask
        (fig//build-fake-chat-prompt st)
        (lambda (msg)
          (fig//fake-chatter-send st msg))
        (format "You are a Twitch chatter named %s talking in LCOLONQ's chat. LCOLONQ streams programming, but the conversation is sometimes off-topic. You are a mostly normal chatter, but you are extremely horny. Your responses are brief, never more than one sentence. You type in all lowercase with no punctuation. You have been banned from the chat multiple times, and you are evading those bans. Most of the time you talk about programming, rarely you act super suspiciously horny." nm)
        "LCOLONQ: what's happening gamers? tonight we're trying to implement"
        "looking cute today"
        )))))

(defconst fig//fake-chatter-profile-drcolon
  (fig//make-fake-chatter-profile
   :username "DrColon"
   :color "#FFFFFF"
   :compute-likeliness (lambda (_) 0.01)
   :send-message
   (lambda (st)
     (fig/ask
      (fig//build-fake-chat-prompt st)
      (lambda (msg)
        (fig//fake-chatter-send st msg))
      (fig//build-fake-chat-system-prompt
       st
       "You are a medical doctor who is enthusiastic about homeopathy and alternative medicine. You are an enthusiastic Gentoo Linux user, and you have more than 20,000 posts on the Gentoo Linux forums.")
      "LCOLONQ: what's happening gamers? tonight we're trying to implement"
      "Have you had your dilutions today?"
      ))))

(setq
 fig//fake-chatters
 (list
  (fig//make-fake-chatter :profile fig//fake-chatter-profile-drcolon)
  ;; (fig//make-fake-chatter :profile fig//fake-chatter-profile-forsen)
  ;; (fig//make-fake-chatter :profile fig//fake-chatter-profile-bigwomenbigfun)
  ;; (fig//make-fake-chatter :profile fig//fake-chatter-profile-whelpless)
  ;; (fig//make-fake-chatter :profile fig//fake-chatter-profile-dansmith87)
  ;; (fig//make-fake-chatter :profile fig//fake-chatter-profile-buffybonnet)
  ;; (fig//make-fake-chatter :profile fig//fake-chatter-profile-chaotix)
  ;; (fig//make-fake-chatter :profile fig//fake-chatter-profile-mountyesfsck)
  ;; (fig//make-fake-chatter :profile fig//fake-chatter-profile-candyboxbox)
  ;; (fig//make-fake-chatter :profile fig//fake-chatter-profile-goofyluffy69)
  ;; (fig//make-fake-chatter :profile fig//fake-chatter-profile-ettelennur)
  ;; (fig//make-fake-chatter :profile fig//fake-chatter-profile-deepwhiffer)
  ))

(provide 'fig-fakechat)
;;; fig-fakechat.el ends here
