;;; fig-bible --- Biblical index -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'dash)
(require 's)
(require 'f)
(require 'ht)

(defvar fig//bible-table nil
  "Hash table mapping (lowercased) words in the Bible to occurences.")

(defun fig//populate-bible-table ()
  "Populate `fig//bible-table' from the Bible text file."
  (let* ((bible-string
          (with-temp-buffer
            (insert-file-contents-literally "~/src/fig/assets/bible.txt")
            (downcase (buffer-string))))
         (bible-string-nosyms (replace-regexp-in-string "[^[:alpha:]]" " " bible-string))
         (bible-words (s-split-words bible-string-nosyms))
         (ret (ht-create)))
    (--each bible-words
      (let ((old (ht-get ret it)))
        (ht-set! ret it (+ 1 (or old 0)))))
    (setf fig//bible-table ret)))
(fig//populate-bible-table)

(defun fig//bible-word-score (word)
  "Return a number between 0.0 and 1.0 representing how biblical WORD is."
  (if (-contains? '("Sam" "Altman") word)
      -666.0
    (let ((occs (ht-get fig//bible-table (downcase (s-trim word))))
          (thresh 0.6))
      (if occs
          (+ thresh (/ (min occs 1000.0) (/ 1000.0 (- 1.0 thresh))))
        0.0))))

(defun fig//bible-word-color (word)
  "Given a WORD, return an appropriate color string."
  (let* ((score (fig//bible-word-score word))
         (others (truncate (+ 128.0 (* 127.0 score)))))
         ;; (others (- 255 (truncate (+ 128.0 (* 127.0 score))))))
    (format "#ff%02x%02x" others others)))
    ;; (format "#00%02x%02x" others others)))

(defun fig//bible-colorize-sentence (sen)
  "Propertize SEN with colors representing word biblicality."
  (let ((ret-score-total 0.0)
        (ret-score-count 0))
    (save-excursion
      (with-temp-buffer
        (insert sen)
        (goto-char (point-min))
        (while (not (eobp))
          (let ((at-word (bounds-of-thing-at-point 'word)))
            (when at-word
              (let* ((word (buffer-substring (car at-word) (cdr at-word)))
                     (score (fig//bible-word-score word))
                     (color (fig//bible-word-color word)))
                (setq ret-score-total (+ ret-score-total score))
                (cl-incf ret-score-count)
                (add-text-properties
                 (car at-word) (cdr at-word)
                 `(face (:foreground ,color))
                 )
                (goto-char (cdr at-word))))
            (when (not (eobp))
              (forward-char 1))))
        (cons (buffer-string) (/ ret-score-total ret-score-count))))))

(provide 'fig-bible)
;;; fig-bible.el ends here
