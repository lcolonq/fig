;;; fig-database --- User database -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'dash)
(require 's)
(require 'f)
(require 'ht)

(defcustom fig/database-path "~/src/fig/database/"
  "Path to the user database."
  :type '(string)
  :group 'fig)

(defun fig//initialize-db ()
  "Initialize the database."
  (mkdir fig/database-path t))

(defun fig//db-path (user)
  "Get the path of the database file for USER."
  (s-concat fig/database-path user))

(defun fig//all-db-users ()
  "Get all users in the database."
  (--map (f-relative it fig/database-path) (f-entries fig/database-path)))

(defun fig//db2-serialize-old-entry (data)
  "Update DATA to not contain structs."
  (--map-when
   (equal (type-of (cdr it)) 'fig//ancestor)
   (cons (car it) (fig//ancestor-serialize (cdr it)))
   (--map-when
    (equal (type-of (cdr it)) 'fig//rpg-character)
    (cons (car it) (fig//rpg-character-serialize (cdr it)))
    data)))

(defun fig//db2-backup-db (user data)
  "Backup the DATA for USER to Redis."
  (let* ((ser (fig//db2-serialize-old-entry data)))
    (fig/db2-set (s-concat "user:" (s-downcase user)) (format "%S" ser))))

(defun fig//save-db (user data)
  "Save DATA for USER."
  (fig//db2-backup-db user data)
  (write-region (format "%S" data) nil (fig//db-path user)))

(defun fig//load-db (user)
  "Load data for USER."
  (when-let* ((path (fig//db-path user))
              (exists (file-exists-p path)))
    (with-temp-buffer
      (insert-file-contents path)
      (read (buffer-string)))))

(defun fig//load-db-entry (user attrib)
  "Load ATTRIB for USER."
  (let ((data (fig//load-db user)))
    (alist-get attrib data)))

(defun fig//set-db-entry (user attrib val)
  "Set ATTRIB to VAL for USER."
  (let ((data (fig//load-db user)))
    (setf (alist-get attrib data) val)
    (fig//save-db user data)))

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

(defvar fig/quotes nil)
(defun fig//save-quotes ()
  "Save the quotes database."
  (fig//save-db "__QUOTES__" fig/quotes))
(defun fig//load-quotes ()
  "Load the quotes database."
  (setf fig/quotes (fig//load-db "__QUOTES__")))
(defun fig//add-quote (user q)
  "Add quote Q from USER."
  (add-to-list 'fig/quotes (cons q user))
  (fig//save-quotes))
(fig//load-quotes)

(defvar fig/recommended-books nil)
(defun fig//save-recommended-books ()
  "Save the quotes database."
  (fig//save-db "__BOOKS__" fig/recommended-books))
(defun fig//load-recommended-books ()
  "Load the quotes database."
  (setf fig/recommended-books (fig//load-db "__BOOKS__")))
(defun fig//add-recommended-book (user b)
  "Add book B from USER."
  (add-to-list 'fig/recommended-books (cons b user))
  (fig//save-recommended-books))
(fig//load-recommended-books)

(provide 'fig-database)
;;; fig-database.el ends here
