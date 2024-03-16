;;; fig-twitch --- Direct Twitch API access -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'dash)
(require 's)
(require 'f)
(require 'ht)
(require 'json)
(require 'request)

(defcustom fig/twitch-avatar-cache-dir "/home/llll/src/fig/assets/avatars/"
  "The directory in which to store downloaded avatar images."
  :type '(string)
  :group 'fig)

(defcustom fig//twitch-api-server "https://api.twitch.tv/helix"
  "Server URL for Twitch API."
  :type '(string)
  :group 'fig)

(defcustom fig//7tv-api-server "https://7tv.io/v3"
  "Server URL for 7TV API."
  :type '(string)
  :group 'fig)

(defvar fig//twitch-last-response nil)
(defvar fig//7tv-last-response nil)
(defvar fig//twitch-vip-list nil)
(defvar fig//7tv-emote-map nil)

(defun fig//twitch-api-get (loc k)
  "Get LOC from the Twitch API, passing the returned JSON to K."
  (request
    (s-concat fig//twitch-api-server loc)
    :type "GET"
    :headers
    `(("Authorization" . ,fig//sensitive-twitch-user-token)
      ("Client-Id" . ,fig//sensitive-twitch-client-id)
      ("Content-Type" . "application/json"))
    :parser #'json-parse-buffer
    :success
    (cl-function
     (lambda (&key data &allow-other-keys)
       (setq fig//twitch-last-response data)
       (funcall k data))))
  t)

(defun fig//twitch-api-post (loc fields k)
  "Post FIELDS to LOC at the Twitch API, passing the returned JSON to K."
  (request
    (s-concat fig//twitch-api-server loc)
    :type "POST"
    :data (json-encode fields)
    :headers
    `(("Authorization" . ,fig//sensitive-twitch-user-token)
      ("Client-Id" . ,fig//sensitive-twitch-client-id)
      ("Content-Type" . "application/json"))
    :parser #'json-parse-buffer
    :error
    (cl-function
     (lambda (&key data &allow-other-keys)
       (print data)
       (message "error")))
    :success
    (cl-function
     (lambda (&key data &allow-other-keys)
       (setq fig//twitch-last-response data)
       (funcall k data))))
  t)

(defun fig//7tv-api-get (loc k)
  "Get LOC from the 7TV API, passing the returned JSON to K."
  (request
    (s-concat fig//7tv-api-server loc)
    :type "GET"
    :headers
    `(("Content-Type" . "application/json"))
    :parser #'json-parse-buffer
    :success
    (cl-function
     (lambda (&key data &allow-other-keys)
       (setq fig//7tv-last-response data)
       (funcall k data))))
  t)

(defun fig//7tv-update-emotes ()
  "Download the current list of 7TV emotes and populate `fig//7tv-emote-map'."
  (fig//7tv-api-get
   (s-concat "/users/twitch/" fig//twitch-broadcaster-id)
   (lambda (data)
     (let* ((emotes (ht-get (ht-get data "emote_set") "emotes")))
       (setq fig//7tv-emote-map (ht-create))
       (--each (seq-into emotes 'list)
         (ht-set! fig//7tv-emote-map (ht-get it "name") (ht-get it "id")))))))
(fig//7tv-update-emotes)

(defun fig//get-7tv-emote (name)
  "Retrieve the 7TV emote ID for NAME."
  (ht-get fig//7tv-emote-map name))

(defun fig//twitch-user-avatar-path (user)
  "Get the path to USER's avatar."
  (s-concat fig/twitch-avatar-cache-dir user ".png"))

(defvar fig//current-stream-title nil)
(defun fig//twitch-get-title (userid k)
  "Get the stream title for USERID and pass it to K."
  (fig//twitch-api-get
   (s-concat "/channels?broadcaster_id=" userid)
   (lambda (data)
     (let ((title (ht-get (aref (ht-get data "data") 0) "title")))
       (funcall k title)))))
(fig//twitch-get-title
 fig//twitch-broadcaster-id
 (lambda (title)
   (setq fig//current-stream-title title)))

(defun fig//twitch-create-redeem (title cost prompt color input)
  "Create a new channel point redeem with TITLE COST PROMPT COLOR and INPUT."
  (fig//twitch-api-post
   (s-concat "/channel_points/custom_rewards?broadcaster_id=" fig//twitch-broadcaster-id)
   `(("title" . ,title)
     ("cost" . ,cost)
     ("prompt" . ,prompt)
     ("background_color" . ,color)
     ("is_user_input_required" . ,input))
   (lambda (data)
     (ignore data)
     (message "Redeem created"))))

(defun fig//twitch-get-user-id (user k)
  "Get the ID for USER and pass it to K."
  (fig//twitch-api-get
   (s-concat "/users?login=" user)
   (lambda (data)
     (let ((id (ht-get (aref (ht-get data "data") 0) "id")))
       (funcall k id)))))

(defun fig//twitch-get-recent-clips (userid k)
  "Get clips from the last week for USERID and pass them to K."
  (fig//twitch-api-get
   (s-concat "/clips?broadcaster_id=" userid)
   (lambda (data)
     (funcall k (seq-map (lambda (it) (ht-get it "url")) (ht-get data "data"))))))

(defun fig//twitch-get-user-recent-clips (user k)
  "Get clips from the last week for USER and pass them to K."
  (fig//twitch-get-user-id
   user
   (lambda (userid)
     (fig//twitch-get-recent-clips userid k))))

(defun fig//twitch-spotlight-streamer (user)
  "Play one of the USERs clips on the model."
  (fig//twitch-get-user-recent-clips
   user
   (lambda (clips)
     (fig//model-region-word "hair" (s-concat user "_"))
     (fig//model-region-word "eyes" "WELCOME")
     (if clips
         (fig//model-region-video "hair" (car clips))
       (fig//model-region-user-avatar "hair" user)))))

(defun fig//twitch-get-user-avatar (user k)
  "Download the avatar for USER and save it to the avatar cache.
K is called when the download is finished."
  (let ((path (fig//twitch-user-avatar-path user)))
    (if (f-exists? path)
        (funcall k)
      (fig//twitch-api-get
       (s-concat "/users?login=" user)
       (lambda (data)
         (let ((url (ht-get (aref (ht-get data "data") 0) "profile_image_url")))
           (fig//write-log (format "downloading avatar: %s %s" url path))
           (make-process
            :name "fig-download-avatar"
            :buffer nil
            :command (list "get_avatar_smol" url path)
            :sentinel
            (lambda (_ _)
              (funcall k)))))))))

(defun fig//twitch-get-vip-list-handler (data)
  "Handle VIP list DATA."
  (let ((inner (ht-get data "data"))
        (pagi (ht-get data "pagination")))
    (seq-map (lambda (it) (push (ht-get it "user_login") fig//twitch-vip-list)) inner)
    (when (and pagi (ht-get pagi "cursor"))
      (fig//twitch-api-get
       (format
        "/channels/vips?broadcaster_id=%s&after=%s"
        fig//twitch-broadcaster-id
        (ht-get pagi "cursor"))
       #'fig//twitch-get-vip-list-handler))))
(defun fig//twitch-get-vip-list ()
  "Fetch current VIP list to `fig//twitch-vip-list'."
  (setq fig//twitch-vip-list nil)
  (fig//twitch-api-get
   (s-concat "/channels/vips?broadcaster_id=" fig//twitch-broadcaster-id)
   #'fig//twitch-get-vip-list-handler)
  t)
(fig//twitch-get-vip-list)

(defun fig//twitch-add-vip (user)
  "Give VIP status to USER."
  (fig/pub '(monitor twitch vip add) (list user)))

(defun fig//twitch-remove-vip (user)
  "Remove VIP status from USER."
  (fig/pub '(monitor twitch vip remove) (list user)))

(defun fig//twitch-remove-random-vip ()
  "Remove VIP status from a random user."
  (let ((user (nth (random (length fig//twitch-vip-list)) fig//twitch-vip-list)))
    (fig//write-chat-event (format "Removing VIP randomly from: %s" user))
    (fig/pub '(monitor twitch vip remove) (list user))))

(defun fig//twitch-shoutout (user)
  "Shoutout USER."
  (fig/pub '(monitor twitch shoutout) (list user)))
(defvar fig//twitch-shoutout-queue nil)
(defun fig//twitch-enqueue-shoutout (user)
  "Queue up a shoutout for USER."
  (push user fig//twitch-shoutout-queue))
(defvar fig//twitch-shoutout-timer nil)
(defun fig//twitch-run-shoutout-timer ()
  "Run the shoutout timer."
  (when fig//twitch-shoutout-timer
    (cancel-timer fig//twitch-shoutout-timer))
  (when-let ((user (pop fig//twitch-shoutout-queue)))
    (fig//twitch-shoutout user))
  (setq
   fig//twitch-shoutout-timer
   (run-with-timer 150 nil #'fig//twitch-run-shoutout-timer)))
(fig//twitch-run-shoutout-timer)

(defvar fig//twitch-current-poll-callback nil
  "A callback that is called and passed the poll winner when the poll concludes.")

(defvar fig//twitch-current-prediction-ids nil
  "Prediction and outcome identifiers for the current prediction.")

(defun fig//twitch-create-poll (title options &optional callback)
  "Create a poll with TITLE and OPTIONS.
CALLBACK will be passed the winner when the poll concludes."
  (unless fig//twitch-current-poll-callback
    (setq fig//twitch-current-poll-callback callback)
    (fig/pub
     '(monitor twitch poll create)
     (list (s-truncate 60 (s-trim title)) options))))

(defun fig//create-prediction (title options)
  "Create a prediction with TITLE and OPTIONS."
  (unless fig//twitch-current-prediction-ids
    (fig/pub '(monitor twitch prediction create) (list title options))))

(defun fig//finish-prediction (outcome)
  "Finish the current prediction with winning OUTCOME."
  (when fig//twitch-current-prediction-ids
    (fig/pub
     '(monitor twitch prediction finish)
     (list (car fig//twitch-current-prediction-ids)
           (car (alist-get outcome (cadr fig//twitch-current-prediction-ids) nil nil #'s-equals?))))))

(defun fig//twitch-say (msg)
  "Write MSG to Twitch chat."
  (let ((trimmed (s-trim msg)))
    (fig//write-chat-message "LCOLONQ" "866686220" trimmed "#616161")
    (fig/pub '(monitor twitch chat outgoing) (list trimmed))))

(defun fig//twitch-handle-message (msg)
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
             text-colored-bible))))
         )
    (fig//assign-chatter-faction user)
    (fig//assign-chatter-ancestor user)
    (fig//assign-chatter-element user)
    (fig//assign-chatter-identity user)
    (fig//assign-chatter-character user)
    (fig//check-chatter-geiser user)
    (fig//hexamedia-update-user user)
    (fig//shindaggers-update-user user)
    (fig//copfish-update-user user)
    (if (s-contains? "SuperIdoldexiaorongdoumeinidetianbayuezhengwudeyangguangdoumeiniyaoyanreai105Cdenididiqingchundezhen" text)
        (cl-incf fig//super-idol-tally)
      (cl-decf fig//super-idol-tally))
    (fig//check-super-idol-tally)
    (push (cons user text) fig//incoming-chat-history)
    (setf (alist-get user fig//chatter-colors nil nil #'s-equals?) color)
    (when (s-equals? user "MODCLONK")
      (fig//obs-log-modclonk-message))
    ;; (fig//friend-judge
    ;;  user userid (s-replace "bald" "ball" text-with-emotes) color
    ;;  (fig//user-sigil user badges))
    (fig//write-chat-message
     user userid
     (s-replace-all
      '(("bald" . "ball")
        ("pokemon" . "pal")
        ("Pokemon" . "Pal")
        ("POKEMON" . "PAL")
        ("pal" . "pokemon")
        ("Pal" . "Pokemon")
        ("PAL" . "POKEMON")
        ("hunter2" . "*******")
        ("*******" . "hunter2"))
      text-with-emotes)
     color
     (fig//user-sigil user badges)
     (cdr text-colored-bible-res))
    (--each fig//chat-commands
      (when (s-contains? (car it) text)
        (funcall (cdr it) user text)))))

(defun fig//twitch-handle-redeem (r)
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

(defconst fig//twitch-redeems
  (list
   (cons "submit headline"
         (lambda (user inp)
           (fig//write-chat-event (format "%s submitted a headline: %s" user inp))
           (fig//friend-journalism user inp)))
   (cons "mental clarity"
         (lambda (user _)
           (fig//write-chat-event (format "%s established mental clarity" user))
           (fig/mental-clarity)))
   (cons "theme: maris-dark"
         (lambda (user _)
           (fig//write-chat-event (format "%s changed the theme: maris-dark" user))
           (fig//change-theme 'ef-maris-dark)))
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
   (cons "run program"
         (lambda (user prog)
           (if (fig//user-authorized user)
               (progn
                 (fig//write-chat-event (s-concat user " runs program: " prog))
                 (fig/bless-eval
                  prog
                  (lambda (x)
                    (-each (cdr x) #'fig/bless-apply-effect)
                    )
                  50))
             (fig//write-chat-event (format "%s is not authorized to run code" user)))))
   (cons "add hook"
         (lambda (user inp)
           (if (fig//user-authorized user)
               (when-let*
                   ((space-pos (string-search " " inp))
                    (h (s-trim (substring inp 0 space-pos)))
                    (prog (s-trim (substring inp space-pos))))
                 (fig//write-chat-event (format "%s adds hook %s: %s" user h prog))
                 (fig//add-bless-hook h prog))
             (fig//write-chat-event (format "%s is not authorized to run code" user)))))
   (cons "feed friend"
         (lambda (user inp)
           (fig//write-chat-event (s-concat user " feeds \"friend\" " inp))
           (fig//friend-feed user inp)))
   (cons "give friend gift"
         (lambda (user inp)
           (if-let ((boosts (fig//load-db-entry user :boost))
                    ((> boosts 0)))
               (progn
                 (fig//write-chat-event (s-concat user " gives \"friend\" a Christmas present: " inp))
                 (fig//friend-gift user inp)
                 (fig//set-db-entry user :boost (- boosts 1)))
             (fig//write-chat-event (s-concat user " doesn't have enough boosts to give one to \"friend\"")))))
   (cons "tfig dneirf evig"
         (lambda (user inp)
           (if-let ((boosts (fig//load-db-entry user :boost))
                    ((< boosts 0)))
               (progn
                 (fig//write-chat-event (reverse (s-concat user " gives \"friend\" a Christmas present: " inp)))
                 (fig//friend-tfig user inp)
                 (fig//set-db-entry user :boost (+ boosts 1)))
             (fig//write-chat-event (reverse (s-concat user " doesn't have enough boosts to give one to \"friend\""))))))
   (cons "talk to friend"
         (lambda (user inp)
           (fig//write-chat-event (s-concat user " talks to \"friend\": " inp))
           (fig//friend-chat user inp)))
   (cons "show friend wikipedia page"
         (lambda (user inp)
           (fig//write-chat-event (s-concat user " shows \"friend\" a Wikipedia page: " inp))
           (fig//friend-react-wikipedia user inp)))
   (cons "friend composes song"
         (lambda (user inp)
           (fig//write-chat-event (s-concat user " asks \"friend\" to compose a song about: " inp))
           (fig//friend-compose-song inp)))
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
   (cons "MODCLONK LAUGH"
         (lambda (user _)
           (fig//write-chat-event "MODCLONK LAUGH DOT OGG")
           (when (-contains? '("LCOLONQ" "MODCLONK") user)
             (soundboard//play-clip "seinfeld.ogg"))))
   (cons "pursue idol dream"
         (lambda (user _)
           (fig//write-chat-event (format "Helping %s pursue their idol dream~" user))
           (fig/chase-dreams)
           (fig//model-region-user-avatar "hair" user)))
   (cons "bells of bezelea"
         (lambda (user msg)
           (muzak//get-song
            msg
            (lambda (song)
              (if song
                  (progn
                    (fig//write-chat-event (format "%s played a song: %s (sponsored by Bezelea)" user msg))
                    (muzak/play-song msg))
                (fig//write-chat-event (format "%s played the bells (sponsored by Bezelea)" user))
                (muzak/play-tracks msg))))))
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
           ;; (soundboard//play-clip "jazz1.ogg")
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
           (soundboard//play-clip "superidol.mp3")
           ;; (soundboard//play-clip "jazz2.ogg")
           ))
   (cons "SEASICKNESS GENERATOR" (lambda (_ _) (fig//model-toggle "zoom_wave")))
   (cons "change the letters"
         (lambda (user inp)
           (fig//write-chat-event (s-concat user " changes the letters: " inp))
           (fig//model-background-text (s-replace " " "" inp))))
   (cons "palette swap (hair)" (fig//handle-redeem-region-swap "hair"))
   (cons "palette swap (highlight)" (fig//handle-redeem-region-swap "highlight"))
   (cons "palette swap (eyes)" (fig//handle-redeem-region-swap "eyes"))
   (cons "palette swap (hat)" (fig//handle-redeem-region-swap "hat"))
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
             (fig//twitch-remove-random-vip))
           (fig//twitch-add-vip user)
           (fig//twitch-get-vip-list)))
   (cons "crown a king and/or queen"
         (lambda (user inp)
           ;; (soundboard//play-clip "girlfriend.ogg")
           (soundboard//play-clip "aeiou.ogg")
           ;; (soundboard//play-clip "jazz3.ogg")
           (fig//write-chat-event (s-concat user " VIPed " inp))
           (when (>= (length fig//twitch-vip-list) 49)
             (fig//twitch-remove-random-vip))
           (fig//twitch-add-vip (string-remove-prefix "@" inp))))
   (cons "deVIPPER"
         (lambda (user inp)
           (fig//write-chat-event (s-concat user " removed VIP from " inp))
           (fig//twitch-remove-vip (string-remove-prefix "@" inp))))
  ))

(provide 'fig-twitch)
;;; fig-twitch.el ends here
