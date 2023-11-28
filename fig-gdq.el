;;; fig-gdq --- Automatic GDQ Donation Messages -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'dash)
(require 's)
(require 'f)
(require 'ht)
(require 'cl-lib)

(defvar fig//gdq-usernames
  (list
   "SpeedyGonzales42"
   "Bezelea"
   "Hat_Knight"
   "DanktownBunny"
   "DrawThatRedstone"
   "han_bun_"
   "neunmalelf"
   "vesdeg"
   "GenDude"
   "nem_dev"
   "LederHosenCowboy"
   "vasher_1025"
   "mickynoon"
   "TF_TOKYO"
   "Setolyx"))

(defvar fig//gdq-templates
  (list
    "We have a $100 dollar donation from goofyluffy69"
    "Greetings from Germany"
    "First time donator"
    "Long time watcher"
    "I'm happy to donate to such a great cause"
    "I am donating because my son just died after a long battle with cancer."
    "WarioWare Smooth Moves has always been my favorite childhood game and I love seeing it get destroyed."
    "Keep up the good work."
    "This goes to naming the Starfield file cumboy"
    "HYYYYYYYYPE!"
    "Much love to the runner and the couch"
    "Save the frames!"
    "Save the animals!"
    "Kill the frames!"
    "Kill the animals!"))

(defun fig//generate-gdq-username (k)
  "Generate a random GDQ username and pass it to K."
  (fig/ask
   "go"
   (lambda (msg)
     (funcall k msg))
   "Generate a random username that might be used by a Games Done Quick donator."
   (list "go" "go" "go")
   (take 3 (fig//shuffle-seq fig//gdq-usernames))))

(defun fig//generate-gdq-message (user k)
  "Generate a random GDQ-style donation message for USER and pass it to K."
  (let ((exuser (nth (random fig//gdq-usernames) fig//gdq-usernames))
        (extemplate (s-join " " (-take 3 (fig//shuffle-seq fig//gdq-templates)))))
    (fig/ask
     user
     (lambda (msg)
     (funcall k msg))
     "Produce a Games Done Quick style donation message from the given username. The format should be \"username|donation amount|message. message. message.\". The message should be between 2 and 5 sentences. The message should be longer than a single sentence."
     exuser
     (format "%s|$%s|%s" exuser (* 25 (random 41)) extemplate))))

(defun fig/ldq ()
  "LCOLONQ Done Quickly."
  (fig//generate-gdq-username
   (lambda (user)
     (fig//generate-gdq-message
      user
      (lambda (s)
        (let ((sp (s-split "|" s)))
          (fig//write-chat-event (format "%s donated %s!" (car sp) (cadr sp)))
          (fig/say
           (format
            "%s donated %s with the message: %s"
            (car sp)
            (cadr sp)
            (caddr sp)))))))))

(provide 'fig-gdq)
;;; fig-gdq.el ends here
