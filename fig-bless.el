;;; fig-bless --- A blessing -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'dash)
(require 's)
(require 'f)
(require 'ht)
(require 'cl-lib)

(defvar fig//bless-current-fuel 0)

(defun fig//bless-helper-literal (x)
  "Return a program pushing X."
  (lambda (s) (cons x s)))

(defun fig//bless-helper-unary (f)
  "Return a program wrapping the unary function F."
  (lambda (s) (cons (funcall f (car s)) (cdr s))))

(defun fig//bless-helper-binary (f)
  "Return a program wrapping the binary function F."
  (lambda (s) (cons (funcall f (cadr s) (car s)) (cddr s))))

(defvar fig//bless-dictionary
  (list
   (cons 'eval (lambda (s) (funcall (car s) (cdr s))))
   (cons 'id (lambda (s) s))
   (cons
    'if
    (lambda (s)
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
   (cons 'explode (lambda (s) (append (car s) (cdr s))))
   (cons 'gather (lambda (s) (-take (car s) (cdr s))))
   (cons '+ (fig//bless-helper-binary '+))
   (cons '- (fig//bless-helper-binary '-))
   (cons '* (fig//bless-helper-binary '*))
   (cons '/ (fig//bless-helper-binary '/))
   (cons '= (fig//bless-helper-binary 'equal))
   (cons '< (fig//bless-helper-binary '<))
   (cons '> (fig//bless-helper-binary '>))
   (cons '<= (fig//bless-helper-binary '>=))
   (cons '>= (fig//bless-helper-binary '>=))
   (cons 'dup (lambda (s) (cons (car s) s)))
   (cons 'swap (lambda (s) (cons (cadr s) (cons (car s) (cddr s)))))
   (cons 'print (lambda (s) (print (car s)) (cdr s)))))

(defun fig//bless-term (term)
  "Transmute TERM according to the nature of the blessing."
  (cond
   ((or (null term) (stringp term) (numberp term))
    (fig//bless-helper-literal term))
   ((consp term)
    (fig//bless-helper-literal (fig//bless term)))
   ((symbolp term)
    (alist-get term fig//bless-dictionary (lambda (s) s)))
   (t (lambda (s) s))))

(defun fig//bless-compose (p1 p2)
  "Compose P1 and P2."
  (lambda (s) (funcall p2 (funcall p1 s))))

(defun fig//bless (terms)
  "Given a list of TERMS, create a program."
  (-reduce #'fig//bless-compose (-map #'fig//bless-term terms)))

(defun fig//bless-run (fuel p &rest stack)
  "Run the program P on STACK.
The program can only use FUEL."
  (let ((fig//bless-current-fuel fuel))
    (funcall p stack)))

(provide 'fig-bless)
;;; fig-bless.el ends here
