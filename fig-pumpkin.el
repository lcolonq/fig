;;; fig-mansion --- living in a mansion with the clonkheads -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'dash)
(require 's)
(require 'f)
(require 'ht)
(require 'cl-lib)

(defun fig//reload-pumpkin ()
  "Reload the pumpkin on the avatar."
  (fig/pub '(avatar pumpkinreload) (list)))

(defun fig//new-pumpkin (k)
  "Start carving a new pumpkin.
Call K when complete."
  (make-process
   :name "fig-new-pumpkin"
   :buffer nil
   :command (list "/home/llll/src/pumpkin/table/newpumpkin.sh")
   :sentinel
   (lambda (_ _)
     (funcall k))
   ))

(defun fig//pumpkin-carve (x y emote k)
  "Carve EMOTE into the pumpkin at (X, Y).
Call K when complete."
  (let* ((eid (fig//get-emote emote))
         (7id (fig//get-7tv-emote emote))
         (epath
          (cond
           (eid (fig//emote-path eid))
           (7id (fig//7tv-emote-path 7id))
           (t nil))))
    (message (format "Carving emote path: %s" epath))
    (when epath
      (make-process
       :name "fig-pumpkin-carve"
       :buffer nil
       :command (list "/home/llll/src/pumpkin/table/knife.sh" epath (format "%s" x) (format "%s" y))
       :sentinel
       (lambda (_ _)
         (funcall k))))))

(provide 'fig-pumpkin)
;;; fig-pumpkin.el ends here
