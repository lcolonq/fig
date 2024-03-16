;;; fig-hexamedia --- Hexamedia interface -*- lexical-binding: t; -*-
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

(defcustom fig//hexamedia-server "https://hexa.media"
  "Server URL for Hexamedia."
  :type '(string)
  :group 'fig)

(defvar fig//hexamedia-last-response nil)

(defun fig//hexamedia-get (loc k)
  "Get LOC from Hexamedia, passing the returned HTML to K."
  (setf request-message-level -1)
  (request
    (s-concat fig//hexamedia-server loc)
    :type "GET"
    :parser (lambda () (libxml-parse-html-region (point-min) (point-max)))
    :success
    (cl-function
     (lambda (&key data &allow-other-keys)
       (setq fig//hexamedia-last-response data)
       (funcall k data))))
  t)

(defun fig//hexamedia-parse-binder-string (bs)
  "Given BS, return the number of obtained cards."
  (let ((match
         (s-match-strings-all
          (rx
           string-start
           (group (zero-or-more (or alnum space)))
           " Set ("
           (group (one-or-more digit))
           )
          bs)))
    (cons (cadar match) (string-to-number (caddar match)))))

(defun fig//hexamedia-get-card-totals (user k)
  "Retrieve the Hexamedia binder for USER.
Pass the resulting card totals to K."
  (fig//hexamedia-get
   (s-concat "/binder/" (s-downcase user))
   (lambda (p)
     (funcall
      k
      (-map
       #'fig//hexamedia-parse-binder-string
       (-filter
        #'stringp
        (--map
         (caddr it)
         (dom-by-tag p 'center))))))))

(defvar fig//hexamedia-user-cache nil)
(defun fig//hexamedia-update-user (user)
  "Update USER data from Hexamedia."
  (unless (-contains? fig//hexamedia-user-cache user)
    (add-to-list 'fig//hexamedia-user-cache user)
    (fig//hexamedia-get-card-totals
     user
     (lambda (ct)
       (fig//set-db-entry user :hexamedia-cards ct)))))

(provide 'fig-hexamedia)
;;; fig-hexamedia.el ends here
