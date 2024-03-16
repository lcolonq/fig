;;; fig-genealogy --- Colonq Family Genealogy -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'dash)
(require 's)
(require 'f)
(require 'ht)
(require 'cl-lib)

(load "fig-genealogy-data.el")

(cl-defstruct
    (fig//ancestor
     (:constructor fig//make-ancestor))
  title
  firstname
  middlename
  nickname
  namesake
  job
  dateofbirth
  dateofdeath
  causeofdeath)

(defun fig//ancestor-serialize (a)
  "Serialize an ancestor A."
  (list
   (fig//ancestor-title a)
   (fig//ancestor-firstname a)
   (fig//ancestor-middlename a)
   (fig//ancestor-nickname a)
   (fig//ancestor-namesake a)
   (fig//ancestor-job a)
   (fig//ancestor-dateofbirth a)
   (fig//ancestor-dateofdeath a)
   (fig//ancestor-causeofdeath a)))

(defun fig//generate-name (era)
  "Determine an appropriate name from ERA."
  (let ((names (alist-get era fig//era-names)))
    (nth (random (length names)) names)))

(defun fig//generate-job (era)
  "Determine an appropriate job from ERA."
  (let ((jobs (alist-get era fig//era-jobs)))
    (nth (random (length jobs)) jobs)))

(defun fig//generate-nickname (era)
  "Determine an appropriate job from ERA."
  (let ((nicknames (alist-get era fig//era-nicknames)))
    (nth (random (length nicknames)) nicknames)))

(defun fig//generate-birth-year ()
  "Determine which year the astrally-focused ancestor was born."
  (+ 1000 (random 1000)))

(defun fig//year-era (year)
  "Determine the era for YEAR."
  (cond
   ((< year 1200) 'medievaltimes)
   ((< year 1400) 'ageofsail)
   ((< year 1800) 'renaissance)
   ((< year 1900) 'steampunk)
   (t 'modern)))

(defun fig//decide-title (era job)
  "Determine the title for JOB in ERA."
  (cl-case era
    (medievaltimes (alist-get job fig//medieval-titles nil nil #'s-equals?))
    (steampunk "Sir")
    (t nil)))

(defun fig//generate-cause-of-death (anc k)
  "Determine ANC's cause of death and pass it to K."
  (fig/ask
   (fig//describe-ancestor-short anc)
   k
   "Given a description of a fictional person, invent a plausible cause of death. The output should be no more than a single clause."
   "Kingkaliente Vasher_1025 \"Grimaldi\" Colonq
Born 1429
Died 1519
Employed as: painting hanger"
   "fell off a ladder"))

(defun fig//generate-ancestor (user k)
  "Search the genealogical record to find USER's namesake and pass the result to K."
  (let* ((birthyear (fig//generate-birth-year))
         (era (fig//year-era birthyear))
         (job (fig//generate-job era))
         (has-nickname (= 0 (random 10)))
         (nickname (when has-nickname (fig//generate-nickname era)))
         (ret
          (fig//make-ancestor
           :title (fig//decide-title era job)
           :nickname nickname
           :namesake user
           :job job
           :dateofbirth birthyear
           :dateofdeath (+ birthyear (random 100))
           :firstname (fig//generate-name era))))
    (if (= 0 (random 2))
        (fig//generate-cause-of-death
         ret
         (lambda (causeofdeath)
           (setf (fig//ancestor-causeofdeath ret) causeofdeath)
           (funcall k ret)))
      (setf (fig//ancestor-causeofdeath ret) "unknown")
      (funcall k ret)))
  nil)

(defun fig//ancestor-name (anc)
  "Return the full name of ANC."
  (s-concat
   (if-let ((tit (fig//ancestor-title anc))) (s-concat tit " ") "")
   (fig//ancestor-firstname anc) " "
   (s-titleize (fig//ancestor-namesake anc)) " "
   (if-let ((nn (fig//ancestor-nickname anc))) (s-concat "\"" nn "\" ") "")
   "Colonq"
   ))

(defun fig//describe-ancestor-short (anc)
  "Describe ANC."
  (s-concat
   (fig//ancestor-name anc) "\n"
   (format "Born %s\n" (fig//ancestor-dateofbirth anc))
   (format "Died %s\n" (fig//ancestor-dateofdeath anc))
   (format "Employed as: %s\n" (fig//ancestor-job anc))))

(defun fig//describe-ancestor (anc)
  "Describe ANC."
  (s-concat
   (fig//describe-ancestor-short anc)
   (format "Cause of death: %s\n" (fig//ancestor-causeofdeath anc))))

(defun fig//get-chatter-ancestor (user)
  "Get the ancestor for USER."
  (fig//load-db-entry user :ancestor))
(defun fig//set-chatter-ancestor (user anc)
  "Get the ancestor for USER to ANC."
  (fig//update-db-default user :ancestor (lambda (_) anc) nil))
(defun fig//assign-chatter-ancestor (user)
  "If USER doesn't have an ancestor, assign them an ancestor."
  (unless (fig//get-chatter-ancestor user)
    (fig//generate-ancestor
     user
     (lambda (anc) (fig//set-chatter-ancestor user anc)))))

(provide 'fig-genealogy)
;;; fig-genealogy.el ends here
