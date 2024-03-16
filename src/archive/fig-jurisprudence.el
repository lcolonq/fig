;;; fig-jurisprudence --- "friend" learns about the law -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:
(require 'dash)
(require 's)
(require 'f)
(require 'ht)
(require 'cl-lib)

(defun fig//friend-judge-evil (user userid text color sigil)
  "Render a guilty verdict.
The relevant strings are USER USERID TEXT COLOR SIGIL."
  (fig//write-chat-message
   user userid text color sigil nil
   (fig//get-twitch-chat-buffer "*fig-evil*")))
(defun fig//friend-judge-good (user userid text color sigil)
  "Render an innocent verdict.
The relevant strings are USER USERID TEXT COLOR SIGIL."
  (fig//write-chat-message
   user userid text color sigil nil
   (fig//get-twitch-chat-buffer "*fig-good*")))

(defun fig//friend-jurisprudence (user userid text color sigil)
  "Judge a chat message.
The relevant strings are USER USERID TEXT COLOR SIGIL."
  (fig/ask
   (format "%s: %s" user text)
   (lambda (data)
     (when-let*
         ((sp (s-split-up-to "|" data 1))
          (cmt (cadr sp))
          (trimmed (s-trim cmt)))
       (fig//friend-say cmt))
     (if (s-contains? "evil" (car (s-split-up-to "|" data 1)))
         (fig//friend-judge-evil user userid text color sigil)
       (fig//friend-judge-good user userid text color sigil)))
   "You are the personality of a desktop buddy named \"friend\". \"friend\" is irreverant but kind, and only speaks in lowercase. You are kind of dumb in a cute way and silly like a virtual pet. You like people, video games, emojis, learning, and food. Today you are serving as a judge in LCOLONQ's stream, determining whether chat messages are good or evil. You live in the corner of LCOLONQ's stream and judge chat messages. Given an emotional state and a user's chat message, please respond with a either the string \"good\" or \"evil\" depending on the message and considering your emotional state. You may also append a | followed by justification for your verdict to your response."
   (list
    "happy | DestinyWaits: love friend"
    "neutral | prodzpod: skidibi gyatt phantom"
    "furious | acher0_: why no leetcode"
    "joyful | LeadenGin: Sam Altman"
    "angry | mickynoon: lcolonGreen"
    )
   (list
    "good | aw i love u 2"
    "evil | prodzpod what on earth is this?"
    "evil | I'm mad so you are evil"
    "evil"
    "good"
    )
  ))

(defun fig//friend-judge (user userid text color sigil)
  "Judge a chat message with some help from counsel.
The relevant strings are USER USERID TEXT COLOR SIGIL."
  (unless (fig//friend-jurisprudence user userid text color sigil)
    (if (= (random 2) 0)
        (fig//friend-judge-evil user userid text color sigil)
      (fig//friend-judge-good user userid text color sigil))))

(provide 'fig-jurisprudence)
;;; fig-jurisprudence.el ends here
