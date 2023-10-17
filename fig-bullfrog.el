;;; fig-bullfrog --- Interaction with the outside world -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'dash)
(require 's)
(require 'f)
(require 'ht)
(require 'json)
(require 'request)

(defcustom fig//bullfrog-api-server "https://colonq.computer/bullfrog/api"
  "Server URL for Bullfrog API."
  :type '(string)
  :group 'fig)

(defvar fig//bullfrog-last-response nil)

(defun fig//bullfrog-get (key k)
  "Get KEY from the Bullfrog store, passing the returned string to K."
  (request
    (s-concat fig//bullfrog-api-server "/get/" key)
    :type "GET"
    :parser #'buffer-string
    :success
    (cl-function
     (lambda (&key data &allow-other-keys)
       (setq fig//bullfrog-last-response data)
       (funcall k data))))
  t)

(defun fig//bullfrog-set (key val)
  "Set KEY to VAL in the Bullfrog store."
  (request
    (s-concat fig//bullfrog-api-server "/set/" key "?token=" fig//bullfrog-token)
    :type "PUT"
    :data val
    :parser #'buffer-string)
  t)

(provide 'fig-bullfrog)
;;; fig-bullfrog.el ends here
