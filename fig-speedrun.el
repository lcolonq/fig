;;; fig-speedrun --- Speedrun -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'dash)
(require 's)
(require 'f)
(require 'ht)

(defcustom fig/stopwatch-buffer "*fig-stopwatch*"
  "Name of buffer used to store stopwatch output."
  :type '(string)
  :group 'fig)

(defconst fig//speedrun-route
  '(("Mat" . 1000)
    ("Pat" . 1000)))

(define-derived-mode fig/stopwatch-mode special-mode "My Insane Pace"
  "Major mode for displaying speedrun stopwatch."
  :group 'fig)

(defface fig/stopwatch-big
  '((t
     :foreground "darkgreen"
     :weight bold
     :height 500
     ))
  "Face for big green livesplit stopwatch."
  :group 'fig)

(defface fig/stopwatch-split-name
  '((t
     :foreground "white"
     :weight bold
     ))
  "Face for stopwatch split name."
  :group 'fig)

(defface fig/stopwatch-split-name-active
  '((t
     :foreground "white"
     :background "darkgreen"
     :weight bold
     ))
  "Face for stopwatch split name."
  :group 'fig)

(defun fig//get-stopwatch-buffer ()
  "Return the stopwatch buffer."
  (unless (get-buffer fig/stopwatch-buffer)
    (with-current-buffer (get-buffer-create fig/stopwatch-buffer)
      (fig/stopwatch-mode)))
  (get-buffer fig/stopwatch-buffer))

(defvar fig//current-stopwatch-start nil
  "Time at which the speedrun stopwatch started.")

(defvar fig//current-stopwatch-split 0
  "Currently active split for the stopwatch.")

(defvar fig//current-split-log nil
  "Log of currently completed splits.")

(defun fig//start-stopwatch ()
  "Reset the stopwatch."
  (interactive)
  (setq fig//current-split-log nil)
  (setq fig//current-stopwatch-start (current-time))
  (setq fig//current-stopwatch-split 0)
  (fig//run-stopwatch-timer))

(defun fig//advance-split ()
  "Advance the split."
  (interactive)
  (cl-incf fig//current-stopwatch-split)
  (setq fig//current-split-log (append fig//current-split-log (list (time-since fig//current-stopwatch-start))))
  (when (and (not (nth fig//current-stopwatch-split fig//speedrun-route)) fig//stopwatch-timer)
    (cancel-timer fig//stopwatch-timer)
    (fig//render-stopwatch)))

(defun fig//render-stopwatch ()
  "Render the current stopwatch state to `fig/stopwatch'."
  (with-current-buffer (fig//get-stopwatch-buffer)
    (let* ((inhibit-read-only t)
           (total-split 0)
           (delta (time-subtract (current-time) fig//current-stopwatch-start)))
      (erase-buffer)

      (--each (-zip fig//speedrun-route (-iota (length fig//speedrun-route)))
        (setf total-split (+ total-split (cdar it)))
        (fig//write-line
         (format
          "%-15s %20s"
          (caar it)
          (format
           "%s %s"
           (let ((sp (nth (cdr it) fig//current-split-log)))
             (if sp
                 (format "%.2d" (- (float-time sp) total-split))
               ""))
           (format-seconds "%02h:%02m:%02s" total-split))
          )
         (if (= fig//current-stopwatch-split (cdr it))
             'fig/stopwatch-split-name-active
           'fig/stopwatch-split-name))
        )

      (fig//write-line
       (format
        "%s%.2d"
        (format-time-string "%M:%S.%1N" delta)
        (random 99))
       'fig/stopwatch-big)

      (goto-char (point-min))
      )))

(defvar fig//stopwatch-timer nil)
(defun fig//run-stopwatch-timer ()
  "Run the stopwatch timer."
  (when fig//stopwatch-timer
    (cancel-timer fig//stopwatch-timer))
  (fig//render-stopwatch)
  (setq
   fig//stopwatch-timer
   (run-with-timer 0.1 nil #'fig//run-stopwatch-timer)))

(provide 'fig-speedrun)
;;; fig-speedrun.el ends here
