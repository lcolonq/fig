;;; fig-gamba --- Don't get it twisted -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'dash)
(require 's)
(require 'f)
(require 'ht)

(defun fig//bait-hn (topic)
  (fig/ask
   topic
   #'message
   "Given a topic, write a hypothetical Hacker News title about that topic. The title should be tongue-in-cheek and humorous. The typical tone of Hacker News titles is laconic and somewhat uninterested and unexcited. The title should be less than 10 words."
   "lisp"
   "Show HN: My (Unfortunate) Lisp Gamedev Experience (2023)"))

(defcustom fig/slots-buffer "*fig-slots*"
  "Name of buffer used to store slot machine output."
  :type '(string)
  :group 'fig)

(define-derived-mode fig/slots-mode special-mode "BONUS BONUS BONUS"
  "Major mode for displaying slot machine."
  :group 'fig)

(defun fig//get-slots-buffer ()
  "Return the slots buffer."
  (unless (get-buffer fig/slots-buffer)
    (with-current-buffer (get-buffer-create fig/slots-buffer)
      (fig/slots-mode)))
  (get-buffer fig/slots-buffer))

(defface fig/slots-main
  '((t
     :foreground "white"
     :weight bold
     :height 500
     ))
  "Face for main row of slot machine."
  :group 'fig)

(defface fig/slots-small
  '((t
     :foreground "white"
     :weight bold
     :height 250
     ))
  "Face for small rows of slot machine."
  :group 'fig)

(defconst fig//slots-reel
  '("7️"
    "🪙"
    "🤖"
    "💻"
    "🍌"
    "🍒"
    ))
(defconst fig//slots-payouts
  '(("7️" . 3000)
    ("🪙". 1000)
    ("🤖". 150)
    ("💻". 150)
    ("🍌". 150)
    ("🍒". 80)
    ))
(defvar fig//slots-current-paid t)
(defvar fig//slots-left-index 0)
(defvar fig//slots-left-target 0)
(defvar fig//slots-left-spinning nil)
(defvar fig//slots-middle-index 0)
(defvar fig//slots-middle-target 0)
(defvar fig//slots-middle-spinning nil)
(defvar fig//slots-right-index 0)
(defvar fig//slots-right-target 0)
(defvar fig//slots-right-spinning nil)

(defun fig//slots-select-targets ()
  "Randomly assign target indices for the reels."
  (setf fig//slots-left-target (+ 10 (random 20)))
  (setf fig//slots-middle-target (+ fig//slots-left-target 10 (random 20)))
  (setf fig//slots-right-target (+ fig//slots-middle-target 10 (random 20))))

(defun fig//slots-check-indices ()
  "Stop reels if they exceed target indices."
  (when (>= (abs fig//slots-left-index) fig//slots-left-target)
    (setf fig//slots-left-spinning nil))
  (when (>= (abs fig//slots-middle-index) fig//slots-middle-target)
    (setf fig//slots-middle-spinning nil))
  (when (>= (abs fig//slots-right-index) fig//slots-right-target)
    (setf fig//slots-right-spinning nil)))

(defun fig//slots-normalize-index (idx)
  "Convert IDX to a valid reel index."
  (let* ((len (length fig//slots-reel))
         (imod (% idx len)))
    (if (< imod 0) (+ imod len) imod)))

(defun fig//slots-get-reel (idx)
  "Return the reel entry for IDX."
  (nth (fig//slots-normalize-index idx) fig//slots-reel))

(defun fig//render-slots ()
  "Render the slot machine buffer."
  (save-excursion
    (with-current-buffer (fig//get-slots-buffer)
      (let* ((inhibit-read-only t))
        (erase-buffer)
        (fig//write
         (format
          " %s    %s     %s\n"
          (fig//slots-get-reel (- fig//slots-left-index 1))
          (fig//slots-get-reel (- fig//slots-middle-index 1))
          (fig//slots-get-reel (- fig//slots-right-index 1))
          )
         'fig/slots-small)
        (fig//write
         (format
          "%s %s %s\n"
          (fig//slots-get-reel fig//slots-left-index)
          (fig//slots-get-reel fig//slots-middle-index)
          (fig//slots-get-reel fig//slots-right-index)
          )
         'fig/slots-main)
        (fig//write
         (format
          " %s    %s     %s\n"
          (fig//slots-get-reel (+ fig//slots-left-index 1))
          (fig//slots-get-reel (+ fig//slots-middle-index 1))
          (fig//slots-get-reel (+ fig//slots-right-index 1))
          )
         'fig/slots-small)
        ))))

(defun fig//slots-pull-lever ()
  "Start the slots spinning."
  (when fig//slots-current-paid
    (setf fig//slots-current-paid nil)
    (setf fig//slots-left-index (random 10))
    (setf fig//slots-middle-index (random 10))
    (setf fig//slots-right-index (random 10))
    (setf fig//slots-left-spinning t)
    (setf fig//slots-middle-spinning t)
    (setf fig//slots-right-spinning t)
    (fig//slots-select-targets)))

(defun fig//slots-payout ()
  "Determine and report the slots payout."
  (let ((lidx (fig//slots-normalize-index fig//slots-left-index))
        (midx (fig//slots-normalize-index fig//slots-middle-index))
        (ridx (fig//slots-normalize-index fig//slots-right-index)))
    (if (or
         (= lidx midx ridx)
         (= (fig//slots-normalize-index (- lidx 1)) midx (fig//slots-normalize-index (+ ridx 1)))
         (= (fig//slots-normalize-index (+ lidx 1)) midx (fig//slots-normalize-index (- ridx 1))))
        (let* ((symbol (fig//slots-get-reel fig//slots-middle-index))
               (prize (alist-get symbol fig//slots-payouts nil nil #'s-equals?)))
          (fig//write-chat-event (format "You won $%s off the slots." prize)))
      (fig//write-chat-event "You lose on the slots!"))
    (setf fig//slots-current-paid t)))
       
(defvar fig//slots-timer nil)
(defun fig//run-slots-timer ()
  "Run the slots timer."
  (fig//slots-check-indices)
  (when fig//slots-timer
    (cancel-timer fig//slots-timer))
  (when fig//slots-left-spinning
    (cl-incf fig//slots-left-index))
  (when fig//slots-middle-spinning
    (cl-incf fig//slots-middle-index))
  (when fig//slots-right-spinning
    (cl-incf fig//slots-right-index))
  (if (or fig//slots-left-spinning fig//slots-middle-spinning fig//slots-right-spinning)
      (fig//render-slots)
    (unless fig//slots-current-paid
      (fig//slots-payout)))
  (setq
   fig//slots-timer
   (run-with-timer 0.1 nil #'fig//run-slots-timer)))
(fig//run-slots-timer)

;; ♠♥♦♣♤♡♢♧
(defconst fig//bj-card-template
  "+-----+
|     |
| %2s  |
|     |
|  %s  |
+-----+")

(defconst fig//bj-suits '("♠" "♥" "♦" "♣"))
(defconst fig//bj-cards '("2" "3" "4" "5" "6" "7" "8" "9" "10" "J" "Q" "K" "A"))
(defconst fig//bj-card-values
  '(("2" . (2))
    ("3" . (3))
    ("4" . (4))
    ("5" . (5))
    ("6" . (6))
    ("7" . (7))
    ("8" . (8))
    ("9" . (9))
    ("10" . (10))
    ("J" . (10))
    ("Q" . (10))
    ("K" . (10))
    ("A" . (1 11))))

(defun fig//bj-render-card (card)
  "Render CARD."
  (format fig//bj-card-template (cdr card) (car card)))

(defun fig//bj-append-card-renders (c1 c2)
  "Combine card renders C1 and C2."
  (s-join "\n" (--zip-with (s-concat it " " other) (s-lines c1) (s-lines c2))))

(defun fig//bj-render-hand (hand)
  "Render HAND."
  (-reduce
   #'fig//bj-append-card-renders
   (-map
    #'fig//bj-render-card
    hand)))

(defun fig//bj-random-card ()
  "Obtain a random card."
  (cons
   (nth (random (length fig//bj-suits)) fig//bj-suits)
   (nth (random (length fig//bj-cards)) fig//bj-cards)))

(defun fig//bj-card-value (card)
  "Compute the values for CARD."
  (alist-get (cdr card) fig//bj-card-values nil nil #'s-equals?))

(defun fig//bj-all-hand-values (hand)
  "Compute all possible values for HAND."
  (if hand
      (let ((vals (fig//bj-card-value (car hand)))
            (rest (fig//bj-all-hand-values (cdr hand))))
        (-uniq
         (-mapcat
          (lambda (val) (--map (+ val it) rest))
          vals)))
    (list 0)))

(defun fig//bj-hand-value (hand)
  "Compute the best value for HAND."
  (let* ((vals (fig//bj-all-hand-values hand))
         (nonlosing (--filter (<= it 21) vals)))
    (if nonlosing
        (-max nonlosing)
      (car vals))))

(defvar fig//bj-stand t)
(defvar fig//bj-current-hand nil)
(defvar fig//bj-dealer-hand nil)

(defun fig//bj-fill-dealer-hand ()
  "Fill the dealer's hand with cards."
  (while (< (fig//bj-hand-value fig//bj-dealer-hand) 17)
    (push (fig//bj-random-card) fig//bj-dealer-hand)))

(defun fig//bj-deal-card ()
  "Deal a card to the player."
  (push (fig//bj-random-card) fig//bj-current-hand))

(defcustom fig/bj-buffer "*fig-bj*"
  "Name of buffer used to store blackjack output."
  :type '(string)
  :group 'fig)

(define-derived-mode fig/bj-mode special-mode "21"
  "Major mode for displaying blackjack."
  :group 'fig)

(defun fig//get-bj-buffer ()
  "Return the blackjack buffer."
  (unless (get-buffer fig/bj-buffer)
    (with-current-buffer (get-buffer-create fig/bj-buffer)
      (fig/bj-mode)))
  (get-buffer fig/bj-buffer))

(defun fig//bj-render ()
  "Render the state of the blackjack game."
  (save-excursion
    (with-current-buffer (fig//get-bj-buffer)
      (let* ((inhibit-read-only t)
             (pval (fig//bj-hand-value fig//bj-current-hand))
             (dval (fig//bj-hand-value fig//bj-dealer-hand)))
        (erase-buffer)
        (fig//write (fig//bj-render-hand fig//bj-current-hand))
        (cond
         ((> pval 21)
          (fig//write-chat-event "Bust! You lose!")
          (fig//write "\nBust! You lose!")
          (setf fig//bj-stand t)
          (fig//bj-start))
         ((> dval 21)
          (fig//write-chat-event "Dealer bust! You win!")
          (fig//write "\nDealer bust! You win!")
          (setf fig//bj-stand t)
          (fig//bj-start))
         ((and fig//bj-stand (> dval pval))
          (fig//write-chat-event (format "Dealer scored %s to your %s! You lose!" dval pval))
          (fig//bj-start))
         ((and fig//bj-stand (> pval dval))
          (fig//write-chat-event (format "Dealer scored %s to your %s! You win!" dval pval))
          (fig//bj-start))
         (fig//bj-stand
          (fig//write-chat-event (format "Dealer scored %s to your %s! It's a tie!" dval pval))
          (fig//bj-start))
         )))))

(defun fig//bj-start ()
  "Start a game of blackjack."
  (when fig//bj-stand
    (setf fig//bj-stand nil)
    (setf fig//bj-current-hand nil)
    (setf fig//bj-dealer-hand nil)
    (fig//bj-fill-dealer-hand)
    (fig//bj-deal-card)
    (fig//bj-deal-card)
    (fig//bj-render)))
(fig//bj-start)

(defun fig//bj-hit ()
  "Blackjack: hit."
  (unless fig//bj-stand
    (fig//bj-deal-card)
    (when (= (fig//bj-hand-value fig//bj-current-hand) 21)
      (setf fig//bj-stand t))
    (fig//bj-render)))

(defun fig//bj-stand ()
  "Blackjack: stand."
  (setf fig//bj-stand t)
  (fig//bj-render))

(provide 'fig-gamba)
;;; fig-gamba.el ends here
