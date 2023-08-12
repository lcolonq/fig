;;; fig-model --- Model controls -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'dash)
(require 's)
(require 'f)
(require 'ht)
(require 'cl-lib)

(defvar fig//model-palette-counter nil "Time to display model changes.")

(cl-defstruct
    (fig//palette
     (:constructor fig//make-palette))
  pedigree
  usage
  name
  hair
  eyes
  highlight
  skin)

(defun fig//read-palette (p)
  "Given a palette expression P, convert to a `fig//palette' struct."
  (fig//make-palette
   :pedigree (nth 0 p)
   :usage (nth 1 p)
   :name (nth 2 p)
   :hair (nth 3 p)
   :eyes (nth 4 p)
   :highlight (nth 4 p)
   :skin (nth 5 p)))

(defun fig//write-palette (p)
  "Given a palette P, convert to a palette expression."
   (list
    (fig//palette-pedigree p)
    (fig//palette-usage p)
    (fig//palette-name p)
    (fig//palette-hair p)
    (fig//palette-eyes p)
    (fig//palette-highlight p)
    (fig//palette-skin p)))

(defvar fig/palettes nil)
(defun fig//save-palettes ()
  "Save the quotes database."
  (fig//save-db "__PALETTES__" (-map #'fig//write-palette fig/palettes)))
(defun fig//load-palettes ()
  "Load the quotes database."
  (setf fig/palettes (-map #'fig//read-palette (fig//load-db "__PALETTES__"))))
(defun fig//get-palette (name)
  "Retrieve the palette named NAME."
  (--find (s-equals? name (fig//palette-name it)) fig/palettes))
(defun fig//add-palette (name hair eyes highlight)
  "Add palette named NAME with colors for HAIR EYES and HIGHLIGHT."
  (unless (fig//get-palette name)
    (add-to-list
     'fig/palettes
     (fig//make-palette
      :name name
      :hair hair
      :eyes eyes
      :highlight highlight)))
  (fig//save-palettes))
(fig//load-palettes)

(defun fig//model-use-palette (pal)
  "Use the palette PAL."
  (fig//model-palette "hair" "lcolonq" (fig//palette-hair pal))
  (fig//model-palette "eyes" "lcolonq" (fig//palette-eyes pal))
  (fig//model-palette "highlight" "lcolonq" (fig//palette-highlight pal)))
(defun fig//model-use-palette-preset (name)
  "Use the palette preset named NAME."
  (when-let ((pal (fig//get-palette name)))
    (setf
     (fig//palette-usage pal)
     (+ 1 (or (fig//palette-usage pal) 0)))
    (fig//save-palettes)
    (fig//model-use-palette pal)))

(defun fig//model-record-change ()
  "Record a change to the model in the counter."
  (setf fig//model-palette-counter 300))

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

(defun fig//model-palette (type msg color)
  "Change the model palette for TYPE to MSG with COLOR."
  (let* ((cleanmsg (s-trim (fig//clean-string msg)))
         (encodedmsg (fig//encode-string cleanmsg))
         (cleancol (s-trim (fig//clean-string color)))
         (encodedcol (fig//encode-string (fig//color-to-html-code cleancol))))
    (unless (or (s-blank? cleanmsg) (s-blank? cleancol))
      (fig//model-record-change)
      (fig/pub '(avatar palette word) (list type encodedmsg))
      (fig/pub '(avatar palette color) (list type encodedcol)))))

(defun fig//model-palette-reset ()
  "Reset the model palette."
  (interactive)
  (fig/pub '(avatar reset)))

(defun fig//model-palette-image (type path)
  "Change the model palette for TYPE to an image at PATH."
  (interactive)
  (let* ((cleanpath (s-trim (fig//clean-string path)))
         (encodedpath (fig//encode-string cleanpath)))
    (unless (s-blank? cleanpath)
      (fig//model-record-change)
      (fig/pub '(avatar palette image) (list type encodedpath)))))

(defun fig//model-further-beyond ()
  "Go even further beyond."
  (interactive)
  (fig//model-palette "eyes" "I" "#00bfff")
  (fig//model-palette "hair" "HAIR" "#ffff00")
  (fig//model-palette "highlight" "hair" "#ffdd00")
  (fig//model-palette "skin" "furtherbeyond" "#ffd1dc"))

(defun fig//model-mordecai-underscore-developer ()
  "Engage mordecai_colonq."
  (interactive)
  (fig//model-palette "eyes" "developer" "darkviolet")
  (fig//model-palette "hair" "mordecai" "black")
  (fig//model-palette "highlight" "mordecai" "white")
  (fig//model-palette "skin" "underscore" "#a56bc7"))

(defun fig//model-yellowberry-suggestion ()
  "Suggested by YellowberryHN."
  (interactive)
  (fig//model-palette "eyes" "lcolonq" "#00ec5c")
  (fig//model-palette "hair" "lcolonq" "#0357d1")
  (fig//model-palette "highlight" "lcolonq" "#05299b")
  (fig//model-palette "skin" "lcolonq" "#ff78e5"))

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

(defun fig//handle-redeem-palette-swap (type defcolor)
  "Return a redeem callback for palette swap of TYPE.
If the color is unspecified, use DEFCOLOR."
  (lambda (user inp)
    (let* ((splinp (s-split-up-to " " inp 1))
           (color (fig//color-to-html-code (car splinp)))
           (text (or (cadr splinp) "lcolonq")))
      (fig//write-chat-event (format "%s changes my %s to %s" user type inp))
      (cond
       (color
        (fig//model-palette type (s-replace " " "" text) color))
       (t
        (fig//model-palette type (s-replace " " "" inp) defcolor))
        ))))

(defvar fig//model-timer nil)
(defun fig//run-model-timer ()
  "Run the model timer."
  (when fig//model-timer
    (cancel-timer fig//model-timer))

  (when fig//model-palette-counter
    (cl-decf fig//model-palette-counter)
    (when (<= fig//model-palette-counter 0)
      (setf fig//model-palette-counter nil)
      (fig//model-palette-reset)))

  (setq
   fig//model-timer
   (run-with-timer 1 nil #'fig//run-model-timer)))
(fig//run-model-timer)

(provide 'fig-model)
;;; fig-model.el ends here
