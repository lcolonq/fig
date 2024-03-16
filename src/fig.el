;;; fig --- Pub/sub bus client -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'dash)
(require 's)
(require 'evil)

(defun fig//discord-say (nm msg)
  "Write MSG to Discord chat as NM."
  (let ((trimmed (s-trim msg)))
    (fig/pub
     '(monitor discord chat outgoing)
     (list
      (base64-encode-string nm t)
      (base64-encode-string trimmed t)))))

(defun fig//discord-handle-message (msg)
  "Write MSG to the chat buffer, processing any commands."
  (let* ((user (fig//decode-string (car msg)))
         (text (fig//decode-string (caddr msg)))
         (text-colored-bible-res (fig//bible-colorize-sentence text))
         (text-colored-bible (car text-colored-bible-res))
         (text-with-emotes
          (s-replace
           "*(this was sent from PowerShell)*"
           fig//powershell-logo
           (fig//add-7tv-emotes text-colored-bible))))
    (fig//write-chat-message
     user "none" (s-replace "bald" "ball" text-with-emotes) nil
     (fig//user-sigil user nil)
     (cdr text-colored-bible-res))))

(defvar fig//event-handlers
  (list
   (cons '(balatro joker) (lambda (msg) (fig//twitch-say "Mr. Green's Power Activated")))
   (cons '(frontend redeem incoming) (lambda (msg) (print msg)))
   (cons '(monitor twitch chat incoming) #'fig//twitch-handle-message)
   ;; (cons '(monitor discord chat incoming) #'fig//discord-handle-message)
   ;; (cons '(monitor irc chat incoming) #'fig//discord-handle-message)
   (cons '(monitor twitch redeem incoming) #'fig//twitch-handle-redeem)
   (cons '(monitor twitch poll begin)
         (lambda (_)
           (fig//write-chat-event "Poll started")
           (fig//friend-respond "The chatters are doing a poll")))
   (cons '(monitor twitch poll end)
         (lambda (msg)
           (let ((winner (car (-max-by (-on #'> #'cadr) (cadr msg)))))
             (fig//write-chat-event (format "Poll finished, winner is: %s" winner))
             (when fig//twitch-current-poll-callback
               (funcall fig//twitch-current-poll-callback winner))
             (setq fig//twitch-current-poll-callback nil))))
   (cons '(monitor twitch prediction begin)
         (lambda (msg)
           (fig//write-chat-event "Gamble started")
           (fig//friend-respond "The chatters are gambling")
           (setq fig//twitch-current-prediction-ids msg)))
   (cons '(monitor twitch prediction end)
         (lambda (_)
           (fig//write-chat-event "Gamble finished")
           (setq fig//twitch-current-prediction-ids nil)))
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
             (fig//bless-hook "follow" (list user))
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

(provide 'fig)
;;; fig.el ends here
