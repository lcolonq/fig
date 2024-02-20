;;; fig-model --- Model controls -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'dash)
(require 's)
(require 'f)
(require 'ht)
(require 'cl-lib)

(defun fig/none-more-verified ()
  "None more verified."
  (interactive)
  (fig/pub '(avatar palette word) (list "eyes" (fig//encode-string "✅")))
  (fig/pub '(avatar palette color) (list "hair" (fig//encode-string (fig//color-value-to-html-code (color-values "purple"))))))

(defun fig//color-value-to-html-code (cval)
  "Convert color value CVAL to an HTML color code."
  (and
   cval
   (format
    "#%02x%02x%02x"
    (truncate (* 255 (/ (car cval) 65535.0)))
    (truncate (* 255 (/ (cadr cval) 65535.0)))
    (truncate (* 255 (/ (caddr cval) 65535.0)))
    )))

(defun fig//color-to-html-code (cname)
  "Convert color name CNAME to an HTML color code."
  (fig//color-value-to-html-code (color-values cname)))

(defvar fig//model-palette-counter nil "Time to display model changes.")

(defun fig//model-record-change ()
  "Record a change to the model in the counter."
  (setf fig//model-palette-counter 300))

(defun fig//model-reset ()
  "Reset the model palette."
  (interactive)
  (fig/pub '(avatar reset)))

(defun fig//model-toggle (toggle)
  "Toggle TOGGLE on model."
  (fig//model-record-change)
  (fig/pub '(avatar toggle) (list toggle)))

(defun fig//model-background-text (msg)
  "Change the background text of the model to MSG."
  (let* ((cleanmsg (s-trim (fig//clean-string msg)))
         (encoded (fig//encode-string cleanmsg)))
    (unless (s-blank? cleanmsg)
      (fig//model-record-change)
      (fig/pub '(avatar text) (list encoded)))))

(cl-defstruct
    (fig//color-source
     (:constructor fig//make-color-source))
  type ;; 'color or 'twitch-emote or '7tv-emote or 'video-url
  value)

(defun fig//string-to-color-source (s)
  "Convert S to a color source."
  (let ((emote (fig//get-emote s))
        (7tv-emote (fig//get-7tv-emote s))
        (color (color-values s))
        (url
         (-contains?
          '("www.youtube.com" "youtube.com" "youtu.be" "www.twitch.tv" "twitch.tv" "clips.twitch.tv")
          (url-host (url-generic-parse-url s)))))
    (cond
     (url (fig//make-color-source :type 'video-url :value s))
     (emote (fig//make-color-source :type 'twitch-emote :value emote))
     (7tv-emote (fig//make-color-source :type '7tv-emote :value 7tv-emote))
     (color (fig//make-color-source :type 'color :value color))
     (t nil))))

(defun fig//model-region-word (type msg)
  "Change the model region TYPE to MSG."
  (let* ((cleanmsg (s-trim (fig//clean-string msg)))
         (encodedmsg (fig//encode-string cleanmsg)))
    (unless (s-blank? cleanmsg)
      (fig//model-record-change)
      (fig/pub '(avatar palette word) (list type encodedmsg)))))

(defun fig//model-region-color (type color)
  "Change the model region TYPE to COLOR."
  (let* ((encodedcol (fig//encode-string (fig//color-value-to-html-code color))))
    (fig//model-record-change)
    (fig/pub '(avatar palette color) (list type encodedcol))))

(defun fig//model-region-image (type path)
  "Change the model region TYPE to an image at PATH."
  (interactive)
  (let* ((cleanpath (s-trim (fig//clean-string path)))
         (encodedpath (fig//encode-string cleanpath)))
    (unless (s-blank? cleanpath)
      (fig//model-record-change)
      (fig/pub '(avatar palette image) (list type encodedpath)))))

(defun fig//model-region-video (type url)
  "Change the model region TYPE to a video at URL."
  (interactive)
  (let* ((cleanurl (s-trim (fig//clean-string url)))
         (encodedurl (fig//encode-string cleanurl)))
    (unless (s-blank? cleanurl)
      (fig//model-record-change)
      (fig/pub '(avatar palette video) (list type encodedurl)))))

(defun fig//model-region-user-avatar (type user)
  "Change the model region TYPE to USER's avatar."
  (fig//twitch-get-user-avatar
   user
   (lambda ()
     (when (f-exists? (fig//twitch-user-avatar-path user))
       (fig//model-region-image type (fig//twitch-user-avatar-path user))))))

(defun fig//model-region-color-source (type cs)
  "Change the model region TYPE to CS."
  (cl-case (fig//color-source-type cs)
    (color
     (fig//model-region-color
      type
      (fig//color-source-value cs)))
    (twitch-emote
     (fig//model-region-image
      type
      (fig//emote-path (fig//color-source-value cs))))
    (7tv-emote
     (fig//model-region-image
      type
      (fig//7tv-emote-path (fig//color-source-value cs))))
    (video-url
     (fig//model-region-video
      type
      (fig//color-source-value cs)))
    (t nil)))

(defvar fig//video-redeem-whitelist
  (list
   "Bezelea"
   "fn_lumi"
   "MxOwlex"
   "NikolaRHristov"
   "goofysystem"
   "MoMoMoVT"
   "SnorlaxBud"
   "GenDude"
   "zulleyy3"
   "freedrull_"
   "theUnseenMystic"
   "MNKN844"
   "fartingle"
   "mickynoon"
   "DJKawaiiFieri"
   "NyxKrage"
   "mawjad_"
   "crane0001"
   "convergent_sequence"
   "nugbones"
   "NineteenNinetyX"
   "fuelsniffer"
   "y1nyng"
   "RoflrawrVT"
   "DFluxStreams"
   "Spaecplex"
   "IcefoxZettai"
   "mTinks"
   "Meowlitary"
   "CamuiCh"
   "joelsgp"
   "redheleftyou"
   "VortisLoste"
   "duxies_"
   "SsJsSs"
   "flyann"
   "Wina"
   "MORGVN_"
   "TheIdOfAlan"
   "body_without_blorgans"
   "cbtcaptain"
   "iLoidtupo"
   "shwasteddd"
   "acher0_"
   "badcop_"
   "Hat_Knight"
   "crazy_stewie"
   "OrdinaryClover_oc"
   "Soymilk"
   "37LN37"
   "Kopadot"
   "Hixrabbit"
   "prodzpod"
   "DocMaho"
   "Celeste_Kyra"
   "fighting_annelids"
   "eientei95"
   "GyrosGeier"
   "FoggyRoses"
   "StuxVT"
   "imgeiser"
   "liquidcake1"
   "4ll4m3nts"
   "Luigi401"
   "sleepysleepy6"
   "TheMaroonHatHacker"
   "CodeSpace0x25"
   "RetroBoi128theGameDev"
   "yellowberryHN"
   "DoctorGlitchy"
   "vvizualizer"
   "KuromaruOniisan"
   "j_art_"
   "BugVT"
   "vchewbah"
   "yiffweed"
   "ESTRE777A"
   "frizst"
   "auts__"
   "floorrip"
   "DestinyWaits"
   "Tomaterr"
   "Ivellon"
   "RyanWinchester_"
   "InspectorDiameter"
   "ryasuar"
   "schizoidcarp"
   "forraz99"
   "The_IronShark"
   "EricAlvin"
   "nichePenguin"
   "fredfuchs_en"
   "Gleil"
   "nightowlmocha"
   "ishishiee"
   "pigeonGuidedMissile"
   ))

(defun fig//user-authorized (user)
  "Return non-nil if USER is authorized to use advanced techniques."
  (let ((boost (fig//load-db-entry user :boost)))
    (or (and boost (> boost 2))
        (and boost (< boost -2))
        (-contains? fig//video-redeem-whitelist user))))
(defun fig//handle-redeem-region-swap (type)
  "Return a redeem callback for region swap of TYPE.
If the color is unspecified, use DEFCOLOR."
  (lambda (user inp)
    (let* ((splinp (s-split-up-to " " (s-trim inp) 1))
           (cs (fig//string-to-color-source (car splinp)))
           (text (if cs (cadr splinp) (s-join " " splinp))))
      (fig//write-chat-event (format "%s changes my %s to %s" user type inp))
      (when cs
        (if (or (fig//user-authorized user)
                (not (eq 'video-url (fig//color-source-type cs))))
            (fig//model-region-color-source type cs)
          (fig//write-chat-event (format "%s is not authorized to play video, boost harder" user))))
      (when text
        (fig//model-region-word type text)))))

(defvar fig//model-timer nil)
(defun fig//run-model-timer ()
  "Run the model timer."
  (when fig//model-timer
    (cancel-timer fig//model-timer))

  (when fig//model-palette-counter
    (cl-decf fig//model-palette-counter)
    (when (<= fig//model-palette-counter 0)
      (setf fig//model-palette-counter nil)
      (fig//model-reset)
      ))

  (setq
   fig//model-timer
   (run-with-timer 1 nil #'fig//run-model-timer)))
(fig//run-model-timer)

(provide 'fig-model)
;;; fig-model.el ends here
