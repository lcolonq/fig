;;; fig-kanban --- Accountability -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'dash)
(require 's)
(require 'f)
(require 'ht)
(require 'cl-lib)

(defvar fig/kanban-columns
  (list
   (list "hello" "world")
   (list "foo" "bar" "baz")
   (list "a")))

(defconst fig/kanban-index
  '(("Queue" . 0)
    ("Active" . 1)
    ("Done" . 2)))

(defcustom fig/kanban-buffer "*fig-kanban*"
  "Name of buffer used to display kanban board."
  :type '(string)
  :group 'fig)

(define-derived-mode fig/kanban-mode tabulated-list-mode "Kanban board"
  "Mode for displaying list of tasks."
  (setq
   tabulated-list-format
   (seq-into (--map (list (car it) 20 nil) fig/kanban-index) 'vector))
  (tabulated-list-init-header))

(defun fig//get-kanban-buffer ()
  "Return the kanban buffer."
  (unless (get-buffer fig/kanban-buffer)
    (with-current-buffer (get-buffer-create fig/kanban-buffer)
      (fig/kanban-mode)))
  (get-buffer fig/kanban-buffer))

(defun fig//render-kanban ()
  "Render the current state to the Kanban buffer."
  (with-current-buffer (fig//get-kanban-buffer)
    (setq
     tabulated-list-entries
     (--map
      (list nil (seq-into it 'vector))
      (apply #'-zip-lists-fill "" fig/kanban-columns)))
    (tabulated-list-print)))

(defun fig/kanban-get-entry ()
  "Get the Kanban entry under point."
  (with-current-buffer (fig//get-kanban-buffer)
    (let* ((props (text-properties-at (point)))
           (col (plist-get props 'tabulated-list-column-name))
           (idx (if col
                    (alist-get col fig/kanban-index nil nil #'s-equals?)
                  (- (length fig/kanban-index) 1)))
           (row (plist-get props 'tabulated-list-entry))
           (tentry (s-trim (seq-elt row idx)))
           (entry (if (s-present? tentry) tentry nil)))
      (list entry idx))))

(defun fig/kanban-move-row (f)
  "Move the Kanban entry under point to a new row computed by F."
  (interactive)
  (let* ((entry (fig/kanban-get-entry))
         (cidx (cadr entry)))
    (when (car entry)
      (let* ((idx (--find-index (s-equals? it (car entry)) (nth cidx fig/kanban-columns)))
             (nidx (funcall f idx))
             (aidx (if (> nidx idx) (- nidx 1) nidx)))
        (setf
         (nth cidx fig/kanban-columns)
         (-insert-at
          aidx
          (car entry)
          (-remove-at
           idx
           (nth cidx fig/kanban-columns))))
        (fig//render-kanban)
        (goto-char (point-min))
        (forward-line aidx)
        (--each (-iota cidx)
          (tabulated-list-next-column))))))

(defun fig/kanban-move-column (f)
  "Move the Kanban entry under point to a new column computed by F."
  (interactive)
  (let* ((entry (fig/kanban-get-entry))
         (idx (cadr entry))
         (new-idx (max (- (length (nth fig/kanban-index idx)) 1) (funcall f idx)))
    (when (car entry)
      (setf
       (nth idx fig/kanban-columns)
       (-remove-item (car entry) (nth idx fig/kanban-columns)))
      (setf
       (nth new-idx fig/kanban-columns)
       (append (nth new-idx fig/kanban-columns) (list (car entry))))
      (fig//render-kanban)
      (goto-char (point-min))
      (forward-line (- (length (nth new-idx fig/kanban-columns)) 1))
      (--each (-iota (max 0 new-idx))
        (tabulated-list-next-column)))))

(defun fig/kanban-move-up ()
  "Move the Kanban entry under point up."
  (interactive)
  (fig/kanban-move-row (lambda (x) (max 0 (- x 1)))))

(defun fig/kanban-move-down ()
  "Move the Kanban entry under point down."
  (interactive)
  (fig/kanban-move-column (lambda (x) (+ x 1))))

(defun fig/kanban-move-left ()
  "Move the Kanban entry under point left."
  (interactive)
  (fig/kanban-move-column (lambda (x) (max 0 (- x 1)))))

(defun fig/kanban-move-right ()
  "Move the Kanban entry under point right."
  (interactive)
  (fig/kanban-move-column (lambda (x) (min (- (length fig/kanban-index) 1) (+ x 1)))))

(defvar-keymap fig/kanban-mode-map
  :suppress t
  "U" #'fig/kanban-move-up
  "D" #'fig/kanban-move-down
  "H" #'fig/kanban-move-left
  "L" #'fig/kanban-move-right
  )
(evil-define-key 'motion fig/kanban-mode-map
  "U" #'fig/kanban-move-up
  "D" #'fig/kanban-move-down
  "H" #'fig/kanban-move-left
  "L" #'fig/kanban-move-right)

(provide 'fig-kanban)
;;; fig-kanban.el ends here
