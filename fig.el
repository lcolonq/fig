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

(defvar fig//current-poll-callback nil
  "A callback that is called and passed the poll winner when the poll concludes.")

(defvar fig//current-prediction-ids nil
  "Prediction and outcome identifiers for the current prediction.")

(defconst fig/host "shiro")
(defconst fig/port 32050)

(defconst fig//event-handlers
  (list
   (cons '(monitor twitch chat incoming) #'fig//handle-chat-message)
   (cons '(monitor twitch redeem incoming) #'fig//handle-redeem)
   (cons '(monitor twitch poll begin)
         (lambda (_)
           (fig//write-chat-event "Poll started")))
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
           (setq fig//current-prediction-ids msg)))
   (cons '(monitor twitch prediction end)
         (lambda (_)
           (fig//write-chat-event "Gamble finished")
           (setq fig//current-prediction-ids nil)))
   ))

(defconst fig//twitch-chat-commands
  (list
   (cons "ResidentSleeper" (lambda (_ _) (soundboard//play-clip "mrbeast.mp3")))
   (cons "roguelike" (lambda (user _) (fig//twitch-say (format "@%s that's not a roguelike" user))))
   (cons "!commands"
         (lambda (_ _)
           (fig//twitch-say
            (s-concat
             "Available commands: "
             (s-join " " (--filter (s-contains? "!" it) (-map #'car fig//twitch-chat-commands)))))))
   (cons "!oomfie" (lambda (_ _) (fig//twitch-say "hi!!!!!!!")))
   (cons "!drink" (lambda (_ _) (fig//twitch-say "its watah im drinkin it")))
   (cons "!discord" (lambda (_ _) (fig//twitch-say "https://discord.gg/f4JTbgN7St")))
   (cons "!specs" (lambda (_ _) (fig//twitch-say "Editor: evil-mode, WM: EXWM, OS: NixOS, hardware: shit laptop")))
   (cons "!coverage" (lambda (_ _) (fig//twitch-say (s-concat "Test coverage: " (number-to-string (random 100)) "%"))))
   (cons "!learnprogramming" (lambda (_ _) (fig//twitch-say "1) program")))
   (cons "!onlyfans" (lambda (_ _) (soundboard//play-clip "pornhub.mp3")))
   (cons "!throne" (lambda (_ _) (fig//twitch-say "xdding")))
   (cons "!vim" (lambda (_ _) (fig//twitch-say "vi is the best text editor, emacs is the best operating system")))
   (cons "!fish" (lambda (_ _) (fig//twitch-say "Not even a nibble...")))
   ))

(defconst fig//twitch-redeems
  (list
   (cons "BOOST"
         (lambda (user _)
           (fig//write-chat-event (s-concat user " boosted their boost number"))
           (fig//update-db-number user :boost (lambda (x) (+ x 1)))))
   (cons "lurker check in" (lambda (user _) (fig//write-chat-event (format "%s says hello" user))))
   (cons "take sip strummer" (lambda (_ _) (fig//write-chat-event "drink water dummy")))
   (cons "deslug" (lambda (_ _) (fig//write-chat-event "unfold your spine")))
   (cons "spinne" (lambda (_ _) (fig//model-toggle "spin")))
   (cons "reverse spinne polarity" (lambda (_ _) (fig//model-toggle "reverse")))
   (cons "super idol" (lambda (_ _) (soundboard//play-clip "superidol.mp3")))
   (cons "SEASICKNESS GENERATOR" (lambda (_ _) (fig//model-toggle "zoom_wave")))
   (cons "change the letters" (lambda (_ inp) (fig//model-background-text (s-replace " " "" inp))))
   (cons "VIPPER"
         (lambda (user _)
           (fig//write-chat-event (s-concat user " VIPed themself"))
           (fig//add-vip user)))
   (cons "crown a king and/or queen"
         (lambda (user inp)
           (fig//write-chat-event (s-concat user " VIPed " inp))
           (fig//add-vip inp)))
   (cons "deVIPPER"
         (lambda (user inp)
           (fig//write-chat-event (s-concat user " removed VIP from " inp))
           (fig//remove-vip inp)))
  ))

(defvar-keymap fig/chat-mode-map
  :suppress t
  "C-l" #'fig//clear-chat
  )
(define-derived-mode fig/chat-mode special-mode "Twitch Chat"
  "Major mode for displaying Twitch chat."
  :group 'fig
  (setq-local window-point-insertion-type t))

(defun fig//write (text &optional face)
  "Write TEXT to the current buffer and apply FACE."
  (let ((text-final (if face (propertize text 'face face) text)))
    (insert text-final)))

(defun fig//write-line (line &optional face)
  "Write LINE and a newline to the current buffer and apply FACE."
  (fig//write (concat line "\n") face))

(defun fig//clean-string (s)
  "Remove special characters from S."
  (replace-regexp-in-string
   "[^[:print:]]" ""
   (replace-regexp-in-string "[^[:ascii:]]" "" s)))

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

(defun fig//create-poll (title options &optional callback)
  "Create a poll with TITLE and OPTIONS.
CALLBACK will be passed the winner when the poll concludes."
  (unless fig//current-poll-callback
    (setq fig//current-poll-callback callback)
    (fig/pub '(monitor twitch poll create) (list title options))))

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

(defun fig//model-toggle (toggle)
  "Toggle TOGGLE on model."
  (fig/pub '(avatar toggle) (list toggle)))

(defun fig//model-background-text (msg)
  "Change the background text of the model to MSG."
  (let ((cleanmsg (s-trim (fig//clean-string msg))))
    (unless (s-blank? cleanmsg)
      (fig/pub '(avatar text) (list (fig//clean-string msg))))))

(defun fig//twitch-say (msg)
  "Write MSG to Twitch chat."
  (fig//write-chat-message "LCOLONQ" msg "#616161")
  (fig/pub '(monitor twitch chat outgoing) (list msg)))

(defun fig//write-chat-event (ev)
  "Write EV to the Twitch chat buffer."
  (let ((inhibit-read-only t))
    (with-current-buffer (fig//get-twitch-chat-buffer)
      (goto-char (point-max))
      (insert (propertize ev 'face 'italic))
      (insert "\n"))))

(defun fig//user-sigil (user &optional badges)
  "Return the sigil character for USER with BADGES."
  (ignore user)
  (cond
   ((-contains? badges "broadcaster/1") "(it me)")
   ((-contains? badges "moderator/1") "⚔")
   ((-contains? badges "vip/1") "💎")
   ((-contains? badges "subscriber/0") "💻")
   ))

(defun fig//write-chat-message (user text &optional color sigil)
  "Write TEXT to the Twitch chat buffer as USER with COLOR."
  (let ((inhibit-read-only t))
    (with-current-buffer (fig//get-twitch-chat-buffer)
      (goto-char (point-max))
      (when sigil (insert sigil) (insert " "))
      (insert (if color (propertize user 'face (list :foreground color)) user))
      (insert ": ")
      (insert text)
      (insert "\n"))))

(defun fig//handle-chat-message (msg)
  "Write MSG to the Twitch chat buffer, processing any commands."
  (fig//write-log msg)
  (let* ((user-data (car msg))
         (user (car user-data))
         (color (when (stringp (cadr user-data)) (cadr user-data)))
         (badges (if (listp (cadr user-data)) (cadr user-data) (caddr user-data)))
         (text (base64-decode-string (cadr msg))))
    (fig//write-chat-message user text color (fig//user-sigil user badges))
    (--each fig//twitch-chat-commands
      (when (s-contains? (car it) text)
        (funcall (cdr it) user text)))))

(defun fig//handle-redeem (r)
  "Handle the channel point redeem R."
  (let* ((user (car r))
         (redeem (cadr r))
         (input (caddr r))
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
