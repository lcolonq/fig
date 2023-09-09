;;; fig-twitch --- Direct Twitch API access -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'dash)
(require 's)
(require 'f)
(require 'ht)
(require 'json)
(require 'request)

(defcustom fig/twitch-avatar-cache-dir "/home/llll/src/fig/avatars/"
  "The directory in which to store downloaded avatar images."
  :type '(string)
  :group 'fig)

(defcustom fig//twitch-api-server "https://api.twitch.tv/helix"
  "Server URL for Twitch API."
  :type '(string)
  :group 'fig)

(defvar fig//twitch-last-response nil)
(defvar fig//twitch-vip-list nil)

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

(defun fig//twitch-user-avatar-path (user)
  "Get the path to USER's avatar."
  (s-concat fig/twitch-avatar-cache-dir user ".png"))

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

(provide 'fig-twitch)
;;; fig-twitch.el ends here
