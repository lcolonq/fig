;;; fig-advents --- Advent of Code API access -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'dash)
(require 's)
(require 'f)
(require 'ht)
(require 'json)
(require 'request)

(defcustom fig//aoc-leaderboard-url "https://adventofcode.com/2023/leaderboard/private/view/3307583.json"
  "URL for Advent of Code API."
  :type '(string)
  :group 'fig)

(defvar fig//aoc-last-response nil)
(defvar fig//aoc-user-stars nil)
(defconst fig//aoc-name-map
  '(("exodrifter_" . "exodrifter")
    ("cephon_altera" . "lainlayer")
    ("monochrome_0" . "monochrome")
    ("yoink2000" . "darius1702")
    ("lukeisun_" . "lukeisun")
    ("dwinkley_" . "dwinkley")
    ("lcolonq" . "llll colonq")
    ("fn_lumi" . "lumi")
    ("leadengin" . "leaden")
    ("vasher_1025" . "vash3r")
    ("andrewdtr" . "drawthatredstone")))

(defun fig//max-aoc-stars ()
  "Return the maximum Advent of Code stars for today."
  (* 2 (string-to-number (format-time-string "%d" (current-time)))))

(defun fig//lookup-aoc-stars (user)
  "Retrieve the Advent of Code stars for USER."
  (let* ((duser (s-downcase user))
         (cuser (s-downcase (alist-get duser fig//aoc-name-map duser nil #'s-equals?))))
    (alist-get cuser fig//aoc-user-stars nil nil #'s-equals?)))

(defun fig//fetch-aoc-api (k)
  "Retrieve the current Advent of Code API.
Pass the resulting JSON to K."
  (request
    fig//aoc-leaderboard-url
    :type "GET"
    :headers
    `(("Cookie" . ,(format "session=%s" fig//sensitive-aoc-session-cookie)))
    :parser #'json-parse-buffer
    :success
    (cl-function
     (lambda (&key data &allow-other-keys)
       (setq fig//aoc-last-response data)
       (funcall k data))))
  nil)

(defun fig//update-aoc-user-stars ()
  "Update the Advent of Code stars list."
  (fig//fetch-aoc-api
   (lambda (data)
     (setf
      fig//aoc-user-stars
      (--map
       (cons (s-downcase (car it)) (cdr it))
       (--filter
        (stringp (car it))
        (--map
         (cons (ht-get it "name") (ht-get it "stars"))
         (ht-values (ht-get data "members")))))))))
(fig//update-aoc-user-stars)

(defvar fig//aoc-day1-input (fig/slurp "aoc/2023/day1.txt"))

(defconst fig//aoc-day1-digit-values
  '(("0" . 0)
    ("1" .  1) ("one" . 1)
    ("2" .  2) ("two" . 2)
    ("3" .  3) ("three" . 3)
    ("4" .  4) ("four" . 4)
    ("5" .  5) ("five" . 5)
    ("6" .  6) ("six" . 6)
    ("7" .  7) ("seven" . 7)
    ("8" .  8) ("eight" . 8)
    ("9" .  9) ("nine" . 9)))
(defun fig//aoc-day1-find-digit (cmp line)
  "Find a digit tehe CMP and LINE are involved somehow please guess."
  (cdr
   (-min-by
    (-on cmp #'car)
    (-filter
     #'car
     (apply
      #'-concat
      (--map
       (-map
        (lambda (ma) (cons (car ma) (cdr it)))
        (s-matched-positions-all (car it) line))
       fig//aoc-day1-digit-values))))))
(defun fig//aoc-day1-line-value (line)
  "Return the value for LINE :3."
  (+
   (* 10 (fig//aoc-day1-find-digit #'>= line))
   (fig//aoc-day1-find-digit #'<= line)))
(defun fig//aoc-day1 ()
  "Solve Advent of Code 2023 Day 1."
  (let ((lines (--filter (not (string-empty-p it)) (s-lines fig//aoc-day1-input))))
    (-sum (-map #'fig//aoc-day1-line-value lines))))

(provide 'fig-advent)
;;; fig-advent.el ends here
