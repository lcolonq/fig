;;; fig-ads --- Advertising -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'dash)
(require 's)
(require 'f)
(require 'ht)
(require 'cl-lib)

(setq image-auto-resize nil)

(defcustom fig/adge-buffer "*fig-adge*"
  "Name of buffer used to display corporations I love personally."
  :type '(string)
  :group 'fig)

(define-derived-mode fig/adge-mode special-mode "friends of the stream"
  "Major mode for displaying corporations."
  :group 'fig
  (setq-local cursor-type nil)
  )

(defun fig//get-adge-buffer ()
  "Return the ads buffer."
  (unless (get-buffer fig/adge-buffer)
    (with-current-buffer (get-buffer-create fig/adge-buffer)
      (fig/adge-mode)
      ))
  (get-buffer fig/adge-buffer))

(defun fig//load-logo (company)
  "Return the logo image for COMPANY."
  (propertize
   company
   'display
   (create-image (format "/home/llll/src/fig/assets/logos/%s.png" company))
   'rear-nonsticky t))

(defconst fig//company-logos
  (list
   (cons "Raytheon" (fig//load-logo "baytheon"))
   (cons "Blackwater" (fig//load-logo "blackwater"))
   (cons "BlackRock" (fig//load-logo "blackrock"))
   (cons "The Pinkertons" (fig//load-logo "pinkertons"))
   (cons "Wizards of the Coast" (fig//load-logo "wizards"))
   (cons "John Madden" (fig//load-logo "madden"))
   (cons "PLUMMCORP" (fig//load-logo "plummcorp"))
   (cons "AT&T" (fig//load-logo "atnt"))
   (cons "Tom Nook" (fig//load-logo "tomnook"))
   (cons "RAID: Shadow Legends" (fig//load-logo "raid"))
   (cons "Blåhaj" (fig//load-logo "blahaj"))
   (cons "Walmart" (fig//load-logo "walmart"))
   (cons "The Shadow Government" (fig//load-logo "shadowgovernment"))
   ))

(defconst fig//ad-templates
  (list
   "In this stream, we stan %s!"
   "Brought to you with no relation to %s."
   "%s generously provided no financial support!"
   "%s is legally a person with rights!"
   "%s makes me jump with joy! 😊"
   "%s declined any comment on sponsorship of this stream."
   ))

(defvar fig//current-ad "Raytheon")

(defun fig//advance-adge ()
  "Choose another random ad."
  (setf
   fig//current-ad
   (car (nth (random (length fig//company-logos)) fig//company-logos)))
  (fig//render-adge))

(defun fig//render-adge ()
  "Render the ads buffer."
  (save-excursion
    (with-current-buffer (fig//get-adge-buffer)
      (let*
          ((inhibit-read-only t))
           (erase-buffer)
           (fig//write (alist-get fig//current-ad fig//company-logos nil nil #'s-equals?))
           (fig//write
            (s-concat
             "\n"
             (format (nth (random (length fig//ad-templates)) fig//ad-templates) fig//current-ad)))
           ))))

(defvar fig//adge-timer nil)
(defun fig//run-adge-timer ()
  "Run the ads timer."
  (when fig//adge-timer
    (cancel-timer fig//adge-timer))
  (fig//advance-adge)
  (setq
   fig//adge-timer
   (run-with-timer 30 nil #'fig//run-adge-timer)))
(fig//run-adge-timer)

(provide 'fig-ads)
;;; fig-ads.el ends here
