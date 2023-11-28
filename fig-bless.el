;;; fig-bless --- A blessing -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'dash)
(require 's)
(require 'f)
(require 'ht)
(require 'cl-lib)

(defvar fig//bless-current-fuel 0)

(defun fig//bless-manage-fuel ()
  "Account for fuel within the bless program."
  (when (<= fig//bless-current-fuel 0)
    (throw 'fig//bless-out-of-fuel 'fig//bless-out-of-fuel))
  (cl-decf fig//bless-current-fuel))

(defun fig//bless-helper-literal (x)
  "Return a program pushing X."
  (lambda (s) (fig//bless-manage-fuel) (cons x s)))

(defun fig//bless-helper-unary (f)
  "Return a program wrapping the unary function F."
  (lambda (s)
    (fig//bless-manage-fuel)
    (cons (funcall f (car s)) (cdr s))))

(defun fig//bless-helper-binary (f)
  "Return a program wrapping the binary function F."
  (lambda (s)
    (fig//bless-manage-fuel)
    (cons (funcall f (cadr s) (car s)) (cddr s))))

(defvar fig//bless-dictionary
  (list
   (cons
    'eval
    (lambda (s)
      (fig//bless-manage-fuel)
      (funcall (car s) (cdr s))))
   (cons
    'id
    (lambda (s)
      (fig//bless-manage-fuel)
      s))
   (cons
    'if
    (lambda (s)
      (fig//bless-manage-fuel)
      (let ((then (caddr s))
            (else (cadr s))
            (c (car s))
            (rest (cdddr s)))
        (if c
            (funcall then rest)
          (funcall else rest)))))
   (cons
    'each
    (lambda (stack)
      (fig//bless-manage-fuel)
      (let ((xs (cadr stack))
            (f (car stack))
            (rest (cddr stack)))
        (-reduce-from
         (lambda (s x)
           (funcall f (cons x s)))
         rest
         xs))))
   (cons '[] (fig//bless-helper-literal nil))
   (cons ': (fig//bless-helper-binary (lambda (xs x) (cons x xs))))
   (cons 'iota (fig//bless-helper-unary (lambda (n) (-iota n))))
   (cons
    'explode
    (lambda (s)
      (fig//bless-manage-fuel)
      (append (car s) (cdr s))))
   (cons
    'gather
    (lambda (s)
      (fig//bless-manage-fuel)
      (-take (car s) (cdr s))))
   (cons '+ (fig//bless-helper-binary '+))
   (cons '- (fig//bless-helper-binary '-))
   (cons '* (fig//bless-helper-binary '*))
   (cons '/ (fig//bless-helper-binary '/))
   (cons '= (fig//bless-helper-binary 'equal))
   (cons '< (fig//bless-helper-binary '<))
   (cons '> (fig//bless-helper-binary '>))
   (cons '<= (fig//bless-helper-binary '>=))
   (cons '>= (fig//bless-helper-binary '>=))
   (cons
    'dup
    (lambda (s)
      (fig//bless-manage-fuel)
      (cons (car s) s)))
   (cons
    'swap
    (lambda (s)
      (fig//bless-manage-fuel)
      (cons (cadr s) (cons (car s) (cddr s)))))
   (cons
    'print
    (lambda (s)
      (fig//bless-manage-fuel)
      (print (car s)) (cdr s)))))

(defun fig//bless-term (term)
  "Transmute TERM according to the nature of the blessing."
  (cond
   ((or (null term) (stringp term) (numberp term))
    (fig//bless-helper-literal term))
   ((consp term)
    (fig//bless-helper-literal (fig//bless term)))
   ((symbolp term)
    (alist-get term fig//bless-dictionary (lambda (s) (fig//bless-manage-fuel) s)))
   (t (lambda (s) (fig//bless-manage-fuel) s))))

(defun fig//bless-compose (p1 p2)
  "Compose P1 and P2."
  (lambda (s) (funcall p2 (funcall p1 s))))

(defun fig//bless (terms)
  "Given a list of TERMS, create a program."
  (-reduce #'fig//bless-compose (-map #'fig//bless-term terms)))

(defun fig/bless (str)
  "Bless STR according to the nature of the blessing."
  (fig//bless (car (read-from-string (s-concat "(" str ")")))))

(defun fig//bless-run (fuel p &optional stack)
  "Run the program P on STACK.
The program can only use FUEL."
  (let ((fig//bless-current-fuel fuel))
    (let ((res (catch 'fig//bless-out-of-fuel (funcall p stack))))
    (if (eq 'fig//bless-out-of-fuel res)
        'out-of-fuel
      res))))

(defvar fig//bless-stack nil)
(defun fig/bless-run (p)
  "Run the program P on the persistent stack."
  (let ((res (fig//bless-run 50 p fig//bless-stack)))
    (if (eq 'out-of-fuel res)
        'out-of-fuel
      (setf fig//bless-stack res)
      (fig//render-bless)
      )))

(defcustom fig/bless-buffer "*fig-bless*"
  "Name of buffer used to ."
  :type '(string)
  :group 'fig)

(define-derived-mode fig/bless-mode special-mode "bless"
  "Major mode for displaying bless."
  :group 'fig
  (setq-local cursor-type nil))

(defun fig//get-bless-buffer ()
  "Return the bless buffer."
  (unless (get-buffer fig/bless-buffer)
    (with-current-buffer (get-buffer-create fig/bless-buffer)
      (fig/bless-mode)))
  (get-buffer fig/bless-buffer))

(defun fig//render-bless ()
  "Render the bless buffer."
  (with-current-buffer (fig//get-bless-buffer)
    (setq-local cursor-type nil)
    (let* ((inhibit-read-only t))
      (erase-buffer)
      (fig//write (format "%s" fig//bless-stack)))))

(provide 'fig-bless)
;;; fig-bless.el ends here
