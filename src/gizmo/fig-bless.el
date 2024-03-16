;;; fig-bless --- A blessing -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'dash)
(require 's)
(require 'f)
(require 'ht)
(require 'cl-lib)
(require 'json)

(defcustom fig/bless-buffer " *fig-bless*"
  "Name of buffer used to store Bless output."
  :type '(string)
  :group 'fig)

(defvar fig//bless-hooks nil)
(defun fig//bless-hook (h &optional stack)
  "Invoke bless hook H on STACK."
  (message "unimplemented"))
(defun fig//add-bless-hook (h str)
  "Compile Bless program STR and set it to hook H."
  (message "unimplemented"))

(defun fig//bless-error (e)
  "Report an error E."
  (message (alist-get 'message e)))

(defun fig//bless-parse-value (j)
  "Construct an Emacs Lisp value representation of the value J."
  (alist-get 'contents j))

(defun fig//bless-parse-effect (j)
  "Construct an Emacs Lisp value representation of the effect J."
  (let ((tag (alist-get 'tag j))
        (c (alist-get 'contents j)))
  (cond
   ((s-equals? tag "EffectPrint") `(print ,(fig//bless-parse-value c)))
   ((s-equals? tag "EffectPrintBackwards") `(print-backwards ,(fig//bless-parse-value c)))
   ((s-equals? tag "EffectSoundboard") `(soundboard ,(fig//bless-parse-value c)))
   ((s-equals? tag "EffectModelToggle") `(model-toggle ,(fig//bless-parse-value c)))
   )))

(defun fig//bless-parse-stack (j)
  "Construct an Emacs Lisp value representation of the stack J."
  (-map #'fig//bless-parse-value j))

(defun fig//bless-parse-effects (j)
  "Construct an Emacs Lisp value representation of the effects J."
  (-map #'fig//bless-parse-effect j))

(defun fig/bless-apply-effect (e)
  "Apply the list of side effects E."
  (cl-case (car e)
    (print (fig//write-chat-event (format "%s" (cadr e))))
    (print-backwards (fig//write-chat-event (reverse (format "%s" (cadr e)))))
    (soundboard (soundboard//play-clip (cadr e)))
    (model-toggle (fig//model-toggle (cadr e)))
    (t (message "Unknown effect tag: %s" (car e)))))

(defun fig/bless-eval (str k &optional fuel)
  "Bless STR according to the nature of the blessing.
Pass the result to K.
Optionally limit evaluation to FUEL steps."
  (let ((buf (generate-new-buffer fig/bless-buffer)))
    (with-current-buffer buf
      (erase-buffer))
    (make-process
     :name "fig-bless-eval"
     :buffer buf
     :command `("bless" "-j" "eval" ,@(if fuel (list "--fuel" (number-to-string fuel)) nil) ,str)
     :sentinel
     (lambda (_ _)
       (let* ((s (with-current-buffer buf (buffer-string)))
              (j (json-read-from-string s))
              (status (alist-get 'status j)))
         (kill-buffer buf)
         (if (s-equals? status "success")
             (funcall
              k
              (cons
               (fig//bless-parse-stack (alist-get 'stack (alist-get 'data j)))
               (fig//bless-parse-effects (alist-get 'effects (alist-get 'data j)))))
           (fig//bless-error (alist-get 'data j))))))))

(provide 'fig-bless)
;;; fig-bless.el ends here
