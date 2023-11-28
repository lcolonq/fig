;;; fig-uwoomfie --- Uwoomfie interface -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'dash)
(require 's)
(require 'f)
(require 'ht)
(require 'rx)
(require 'cl-lib)
(require 'request)
(require 'dom)

(defcustom fig//uwoomfie-server "https://uwoomfie.com"
  "Server URL for Uwoomfie."
  :type '(string)
  :group 'fig)

(defvar fig//uwoomfie-last-response nil)
(defvar fig//uwoomfie-honorary-viewers nil)
(defvar fig//uwoomfie-cool-people nil)

(defun fig//uwoomfie-get (loc k)
  "Get LOC from Uwoomfie, passing the returned HTML to K."
  (request
    (s-concat fig//uwoomfie-server loc)
    :type "GET"
    :parser (lambda () (libxml-parse-html-region (point-min) (point-max)))
    :success
    (cl-function
     (lambda (&key data &allow-other-keys)
       (setq fig//uwoomfie-last-response data)
       (funcall k data))))
  t)

(defun fig//uwoomfie-fetch-honorary-viewers (k)
  "Fetch the list of Uwoomfie honorary viewers and pass them to K."
  (fig//uwoomfie-get
   "/honoraryviewers.html"
   (lambda (data)
     (funcall
      k
      (--map (car (s-split " " (caddr it))) (dom-by-tag data 'h1))))))

(defun fig//uwoomfie-fetch-cool-people (k)
  "Fetch the list of Uwoomfie cool people and pass them to K."
  (fig//uwoomfie-get
   "/coolpeople.html"
   (lambda (data)
     (funcall
      k
      (--map
       (s-replace "@" "" (car (last (s-split "/" (cdaadr it)))))
       (dom-by-tag data 'a))))))

(defun fig//get-uwoomfie-status (user)
  "Return the Uwoomfie status for USER."
  (cond
   ((-contains? fig//uwoomfie-cool-people user) 'cool)
   ((-contains? fig//uwoomfie-honorary-viewers user) 'honored)
   (t nil)))

(fig//uwoomfie-fetch-honorary-viewers
 (lambda (users)
   (setf fig//uwoomfie-honorary-viewers users)))

(fig//uwoomfie-fetch-cool-people
 (lambda (users)
   (setf fig//uwoomfie-cool-people (cons "Watchmakering" users))))

(provide 'fig-uwoomfie)
;;; fig-uwoomfie.el ends here
