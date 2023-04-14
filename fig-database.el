;;; fig-database --- User database -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'dash)
(require 's)
(require 'ht)

(defcustom fig/database-path "~/src/fig/database/"
  "Path to the user database."
  :type '(string)
  :group 'fig)

(defvar fig//database-connection nil
  "The connection to the database.")

(defun fig//initialize-db ()
  "Initialize the database."
  (mkdir fig/database-path t))

(defun fig//db-path (user)
  "Get the path of the database file for USER."
  (s-concat fig/database-path user))
  
(defun fig//save-db (user data)
  "Save DATA for USER."
  (write-region (format "%S" data) nil (fig//db-path user)))

(defun fig//load-db (user)
  "Load DATA for USER."
  (when-let* ((path (fig//db-path user))
              (exists (file-exists-p path)))
    (with-temp-buffer
      (insert-file-contents-literally path)
      (read (buffer-string)))))

(defun fig//update-db (user f)
  "Apply F to the database data for USER."
  (let ((data (fig//load-db user)))
    (fig//save-db user (funcall f data))))

(defun fig//update-db-default (user attrib f default)
  "Apply F to ATTRIB on USER data.
If ATTRIB is not found, default to DEFAULT."
  (fig//update-db
   user
   (lambda (d)
     (let* ((val (alist-get attrib d))
            (v (if val val default))
            (new (funcall f v)))
       (setf (alist-get attrib d) new)
       d))))

(defun fig//update-db-number (user attrib f)
  "Apply F to ATTRIB on USER data.
The value is assumed to be a number."
  (fig//update-db-default user attrib f 0))

(defun fig//update-db-list (user attrib f)
  "Apply F to ATTRIB on USER data.
The value is assumed to be a list."
  (fig//update-db-default user attrib f nil))

(provide 'fig-database)
;;; fig-database.el ends here
