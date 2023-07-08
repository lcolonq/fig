;;; fig-voice --- Voice communication -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 's)

(defcustom fig/transcribe-buffer " *fig-transcribe*"
  "Name of buffer used to store transcription output."
  :type '(string)
  :group 'fig)

(defcustom fig/transcribe-error-buffer " *fig-transcribe-error*"
  "Name of buffer used to store transcription errors."
  :type '(string)
  :group 'fig)

(defcustom fig/ask-buffer " *fig-ask*"
  "Name of buffer used to store ChatGPT output."
  :type '(string)
  :group 'fig)

(defcustom fig/ask-error-buffer " *fig-ask-error*"
  "Name of buffer used to store ChatGPT errors."
  :type '(string)
  :group 'fig)

(defcustom fig/biblical-buffer " *fig-biblical*"
  "Name of buffer used to store ChatGPT output."
  :type '(string)
  :group 'fig)

(defcustom fig/biblical-error-buffer " *fig-biblical-error*"
  "Name of buffer used to store ChatGPT errors."
  :type '(string)
  :group 'fig)

(defvar fig//current-transcribe-process nil)
(defvar-local fig//transcribe-callback nil)

(defvar fig//current-ask-process nil)
(defvar-local fig//ask-callback nil)

(defvar fig//current-biblical-process nil)
(defvar-local fig//biblical-callback nil)

(defun fig/say (msg)
  "Use TTS to say MSG."
  (let ((tmpfile (make-temp-file "fig-say")))
    (with-temp-file tmpfile (insert msg))
    (start-process "fig-say" nil "say" tmpfile)))

(defun fig/ask (question k &optional systemprompt user assistant)
  "Ask QUESTION to ChatGPT and pass the answer to K.
Optionally use SYSTEMPROMPT and the USER and ASSISTANT prompts."
  (unless fig//current-ask-process
    (let ((tmpfile (make-temp-file "fig-ask"))
          (tmpfilesystem (make-temp-file "fig-ask-system")))
      (with-temp-file tmpfile (insert question))
      (when systemprompt
        (with-temp-file tmpfilesystem (insert systemprompt)))
      (with-current-buffer (get-buffer-create fig/ask-buffer)
        (setq-local fig//ask-callback k)
        (erase-buffer))
      (setq
       fig//current-ask-process
       (make-process
        :name "fig-ask"
        :buffer (get-buffer-create fig/ask-buffer)
        :command
        (list
         "chatgpt"
         tmpfile
         (if systemprompt tmpfilesystem "systemprompt.txt")
         (if user user "What's happening tonight, computer?")
         (if assistant assistant "We're chilling and grilling on the computer as usual, LCOLONQ.")
         )
        :stderr (get-buffer-create fig/ask-error-buffer)
        :sentinel
        (lambda (_ _)
          (setq fig//current-ask-process nil)
          (with-current-buffer (get-buffer-create fig/ask-buffer)
            (funcall fig//ask-callback (buffer-string)))))))))

(defun fig/biblical (question k)
  "Ask QUESTION to ChatGPT and pass the answer to K."
  (unless fig//current-biblical-process
    (let ((tmpfile (make-temp-file "fig-biblical")))
      (with-temp-file tmpfile (insert question))
      (with-current-buffer (get-buffer-create fig/biblical-buffer)
        (setq-local fig//biblical-callback k)
        (erase-buffer))
      (setq
       fig//current-biblical-process
       (make-process
        :name "fig-biblical"
        :buffer (get-buffer-create fig/biblical-buffer)
        :command (list "biblical" tmpfile)
        :stderr (get-buffer-create fig/biblical-error-buffer)
        :sentinel
        (lambda (_ _)
          (setq fig//current-biblical-process nil)
          (with-current-buffer (get-buffer-create fig/biblical-buffer)
            (funcall fig//biblical-callback (buffer-string)))))))))

(defun fig/begin-transcribe (k)
  "Start recording audio to transcribe, passing the result to K."
  (unless fig//current-transcribe-process
    (message "Transcribing...")
    (with-current-buffer (get-buffer-create fig/transcribe-buffer)
      (setq-local fig//transcribe-callback k)
      (erase-buffer))
    (setq
     fig//current-transcribe-process
     (make-process
      :name "fig-transcribe"
      :buffer (get-buffer-create fig/transcribe-buffer)
      :command (list "transcribe")
      :stderr (get-buffer-create fig/transcribe-error-buffer)
      :sentinel
      (lambda (_ _)
        (setq fig//current-transcribe-process nil)
        (with-current-buffer (get-buffer-create fig/transcribe-buffer)
          (funcall fig//transcribe-callback (buffer-string))))))))

(defun fig/end-transcribe ()
  "Finish recording transcription audio."
  (interactive)
  (when fig//current-transcribe-process
    (message "End of transcription")
    (start-process "pkill" nil "pkill" "parecord"))
  nil)

(defun fig/transcribe-poll ()
  "Create a Yes/No poll from voice transcription."
  (interactive)
  (fig/begin-transcribe
   (lambda (msg)
     (fig//twitch-say (s-concat "Current poll: " msg))
     (fig//create-poll msg (list "Yes" "No")))))

(defun fig/transcribe-chat ()
  "Send to Twitch chat from voice transcription."
  (interactive)
  (fig/begin-transcribe
   (lambda (msg)
     (fig//twitch-say msg))))

(defhydra fig/voice-hydra (:color blue :hint nil)
  "Voice-related commands"
  ("a" (fig/end-transcribe) "end")
  ("p" (fig/transcribe-poll) "poll")
  ("c" (fig/transcribe-chat) "chat"))

(provide 'fig-voice)
;;; fig-voice.el ends here
