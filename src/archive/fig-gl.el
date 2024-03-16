;;; fig-gl --- It's Triangle Time -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'dash)
(require 's)
(require 'f)
(require 'ht)
(require 'cl-lib)

(cl-defstruct
    (fig//vec3
     (:constructor fig//make-vec3))
  (x 0)
  (y 0)
  (z 0))

(cl-defstruct
    (fig//triangle
     (:constructor fig//make-triangle))
  a
  b
  c)

(defun fig//sign (x)
  "Return -1 if X is negative otherwise 1."
  (if (<= x 0) -1 1))

(defun fig//side-of-line (l0 l1 p)
  "Determine upon which side of the line segment from L0 to L1 lies P."
  (fig//sign
   (-
    (* (- (fig//vec3-x l0) (fig//vec3-x p))
       (- (fig//vec3-y l1) (fig//vec3-y p)))
    (* (- (fig//vec3-x l1) (fig//vec3-x p))
       (- (fig//vec3-y l0) (fig//vec3-y p))))))
  ;; (let ((dx (- (fig//vec3-x l1) (fig//vec3-x l0)))
  ;;       (dy (- (fig//vec3-y l1) (fig//vec3-y l0))))
  ;;   (if (= dx 0)
  ;;       (* (fig//sign dy) (if (> (fig//vec3-x p) (fig//vec3-x l0)) 1 -1))
  ;;     (let* ((m (/ dy dx))
  ;;            (b (- (fig//vec3-y l0) (* m (fig//vec3-x l0))))
  ;;            (pliney (+ (* m (fig//vec3-x p)) b)))
  ;;       (* (fig//sign dx) (if (> (fig//vec3-y p) pliney) -1 1))))))

(defun fig//triangle-contains (tri p)
  "Determine if triangle TRI can be said to contain point P."
  (let* ((a (fig//triangle-a tri))
         (b (fig//triangle-b tri))
         (c (fig//triangle-c tri))
         (needs-swap (= 1 (fig//side-of-line a b c)))
         (p1 a)
         (p2 (if needs-swap c b))
         (p3 (if needs-swap b c))
         (l12 (fig//side-of-line p1 p2 p))
         (l23 (fig//side-of-line p2 p3 p))
         (l31 (fig//side-of-line p3 p1 p)))
    (= -1 l12 l23 l31)))

(defcustom fig/gl-buffer "*fig-gl*"
  "Name of buffer used to display software render."
  :type '(string)
  :group 'fig)

(define-derived-mode fig/gl-mode special-mode "GL"
  "Major mode for displaying software render."
  :group 'fig
  (setq-local cursor-type nil))

(defun fig//get-gl-buffer ()
  "Return the GL buffer."
  (unless (get-buffer fig/gl-buffer)
    (with-current-buffer (get-buffer-create fig/gl-buffer)
      (fig/gl-mode)))
  (get-buffer fig/gl-buffer))

(defun fig//gl-clear ()
  "Clear the GL buffer."
  (with-current-buffer (fig//get-gl-buffer)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (--each (-iota 32)
        (insert "................................................................\n")))))

(defun fig//gl-pixel (p color)
  "Draw a pixel of COLOR at P."
  (ignore color)
  (let ((x (fig//vec3-x p))
        (y (fig//vec3-y p)))
    (unless (or (< x 0) (>= x 32) (< y 0) (>= y 32))
      (with-current-buffer (fig//get-gl-buffer)
        (save-excursion
          (let ((inhibit-read-only t))
            (goto-char (point-min))
            (forward-line (fig//vec3-y p))
            (forward-char (* 2 (fig//vec3-x p)))
            (delete-char 2)
            (insert (propertize "##" 'face `(:foreground ,color)))))))))

(defun fig//gl-draw-triangle (tri)
  "Draw TRI."
  (-each (-iota 32)
    (lambda (x)
      (-each (-iota 32)
        (lambda (y)
          (let ((p (fig//make-vec3 :x x :y y)))
            (when (fig//triangle-contains tri p)
              (fig//gl-pixel p "white")))))))
  (fig//gl-pixel (fig//triangle-a tri) "red")
  (fig//gl-pixel (fig//triangle-b tri) "green")
  (fig//gl-pixel (fig//triangle-c tri) "blue"))

(provide 'fig-gl)
;;; fig-gl.el ends here
