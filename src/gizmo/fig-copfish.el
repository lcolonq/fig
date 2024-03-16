;;; fig-copfish --- Copfish interface -*- lexical-binding: t; -*-
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

(defcustom fig//copfish-server "https://cop.fish/api/"
  "Server URL for Copfish."
  :type '(string)
  :group 'fig)

(defvar fig//copfish-last-response nil)

(defun fig//copfish-get (loc k)
  "Get LOC from Copfish, passing the returned HTML to K."
  (setf request-message-level -1)
  (request
    (s-concat fig//copfish-server loc)
    :type "GET"
    :parser #'buffer-string
    :success
    (cl-function
     (lambda (&key data &allow-other-keys)
       (setq fig//copfish-last-response data)
       (funcall k data))))
  t)

(defun fig//copfish-get-fish (user k)
  "Retrieve USER's fish ratio from copfish API.
Pass the resulting fraction to K."
  (fig//copfish-get
   (s-concat "fishdex/" user)
   (lambda (s)
     (let ((sp (s-split " " s)))
       (when (= (length sp) 2)
         (funcall k (cons (string-to-number (car sp)) (string-to-number (cadr sp)))))))))

(defvar fig//copfish-user-cache nil)
(defun fig//copfish-update-user (user)
  "Update USER data from Copfish."
  (unless (-contains? fig//copfish-user-cache user)
    (add-to-list 'fig//copfish-user-cache user)
    (fig//copfish-get-fish
     user
     (lambda (ct)
       (fig//set-db-entry user :copfish-ratio ct)))))

(provide 'fig-copfish)
;;; fig-copfish.el ends here
