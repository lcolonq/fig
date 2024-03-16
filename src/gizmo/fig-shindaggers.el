;;; fig-shindaggers --- Shindaggers interface -*- lexical-binding: t; -*-
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

(defcustom fig//shindaggers-server "https://shindaggers.io"
  "Server URL for Shindaggers."
  :type '(string)
  :group 'fig)

(defvar fig//shindaggers-last-response nil)

(defun fig//shindaggers-get (loc k)
  "Get LOC from Shindaggers, passing the returned HTML to K."
  (setf request-message-level -1)
  (request
    (s-concat fig//shindaggers-server loc)
    :type "GET"
    :headers
    `(("Content-Type" . "application/json"))
    :parser #'json-parse-buffer
    :success
    (cl-function
     (lambda (&key data &allow-other-keys)
       (setq fig//shindaggers-last-response data)
       (funcall k data))))
  t)

(defun fig//shindaggers-get-user-id (user k)
  "Retrieve the Shindaggers user number for USER and pass it to K."
  (let ((duser (s-downcase user)))
    (fig//shindaggers-get
     (s-concat "/api/users?search=" duser)
     (lambda (p)
       (let ((res
              (car
               (--filter
                (s-equals? (s-downcase (ht-get it "name")) duser)
                (seq-into (ht-get p "Users") 'list)))))
         (funcall k (and res (string-to-number (ht-get res "id")))))))))

(defun fig//shindaggers-get-collection (userid k)
  "Retrieve the Shindaggers collection for USERID and pass it to K."
  (fig//shindaggers-get
   (format "/api/user/%s/collection" userid)
   (lambda (p)
     (funcall
      k
      (--map (ht-get it "name") (ht-get p "Collectables"))))))

(defvar fig//shindaggers-user-cache nil)
(defun fig//shindaggers-update-user (user)
  "Update USER data from Shindaggers."
  (unless (-contains? fig//shindaggers-user-cache user)
    (add-to-list 'fig//shindaggers-user-cache user)
    (fig//shindaggers-get-user-id
     user
     (lambda (userid)
       (fig//shindaggers-get-collection
        userid
        (lambda (knives)
          (fig//set-db-entry user :shindaggers-knives knives)))))))

(provide 'fig-shindaggers)
;;; fig-shindaggers.el ends here
