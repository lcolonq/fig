;;; fig-rpg --- Role playing: gaming! -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'dash)
(require 's)
(require 'ht)
(require 'cl-lib)

(defun fig//roll (sides)
  "Roll a dice with SIDES."
  (1+ (random sides)))

(cl-defstruct
    (fig//rpg-ability-scores
     (:constructor fig//make-rpg-ability-scores))
  (str 0)
  (dex 0)
  (con 0)
  (int 0)
  (wis 0)
  (cha 0))

(defun fig//roll-ability-score ()
  "Generate one ability score."
  (let* ((stats (list (fig//roll 6) (fig//roll 6) (fig//roll 6) (fig//roll 6)))
         (total (- (-sum stats) (-min stats))))
    total))

(defun fig//roll-ability-scores ()
  "Generate ability scores."
  (fig//make-rpg-ability-scores
   :str (fig//roll-ability-score)
   :dex (fig//roll-ability-score)
   :con (fig//roll-ability-score)
   :int (fig//roll-ability-score)
   :wis (fig//roll-ability-score)
   :cha (fig//roll-ability-score)))

(cl-defstruct
    (fig//rpg-saves
     (:constructor fig//make-rpg-saves))
  (wand 0)
  (breath 0)
  (death 0)
  (polymorph 0)
  (spells 0))

(cl-defstruct
    (fig//rpg-class
     (:constructor fig//make-rpg-class))
  name
  ideal-stat
  hit-die
  minimum-ability-scores
  attack-gain
  saves)

(defconst fig/rpg-fighter
  (fig//make-rpg-class
   :name "Fighter"
   :ideal-stat #'fig//rpg-ability-scores-str
   :hit-die 10
   :minimum-ability-scores
   (fig//make-rpg-ability-scores
    :str 9
    :dex 6
    :con 7
    :int 3
    :wis 6
    :cha 6)
   :attack-gain (cons 1 1)
   :saves
   (fig//make-rpg-saves
    :wand 18
    :breath 20
    :death 16
    :polymorph 17
    :spells 19)
   ))

(defconst fig/rpg-cleric
  (fig//make-rpg-class
   :name "Cleric"
   :ideal-stat #'fig//rpg-ability-scores-wis
   :hit-die 8
   :minimum-ability-scores
   (fig//make-rpg-ability-scores
    :str 6
    :dex 3
    :con 6
    :int 6
    :wis 9
    :cha 6)
   :attack-gain (cons 2 3)
   :saves
   (fig//make-rpg-saves
    :wand 14
    :breath 16
    :death 10
    :polymorph 13
    :spells 15)
   ))

(defconst fig/rpg-magic-user
  (fig//make-rpg-class
   :name "Magic User"
   :ideal-stat #'fig//rpg-ability-scores-int
   :hit-die 4
   :minimum-ability-scores
   (fig//make-rpg-ability-scores
    :str 3
    :dex 6
    :con 6
    :int 9
    :wis 6
    :cha 6)
   :attack-gain (cons 2 5)
   :saves
   (fig//make-rpg-saves
    :wand 11
    :breath 15
    :death 14
    :polymorph 13
    :spells 12)
   ))

(defconst fig/rpg-classes
  (list
   fig/rpg-fighter
   fig/rpg-cleric
   fig/rpg-magic-user))

(cl-defstruct
    (fig//rpg-character
     (:constructor fig//make-rpg-character))
  name
  level
  class
  max-hp
  ability-scores)

(defun fig//scores-at-least (scores reqs)
  "Return non-nil if SCORES are all greater than or equal to REQS."
  (and
   (>= (fig//rpg-ability-scores-str scores) (fig//rpg-ability-scores-str reqs))
   (>= (fig//rpg-ability-scores-dex scores) (fig//rpg-ability-scores-dex reqs))
   (>= (fig//rpg-ability-scores-con scores) (fig//rpg-ability-scores-con reqs))
   (>= (fig//rpg-ability-scores-int scores) (fig//rpg-ability-scores-int reqs))
   (>= (fig//rpg-ability-scores-wis scores) (fig//rpg-ability-scores-wis reqs))
   (>= (fig//rpg-ability-scores-cha scores) (fig//rpg-ability-scores-cha reqs))))

(defun fig//pick-class (scores)
  "Given ability SCORES, choose a fitting class."
  (let* ((eligible (--filter
                    (fig//scores-at-least scores (fig//rpg-class-minimum-ability-scores it))
                    fig/rpg-classes))
         (ideal-scores (--map (cons it (funcall (fig//rpg-class-ideal-stat it) scores)) eligible))
         (ideal-class (-max-by (-on #'>= #'cdr) ideal-scores)))
    (car ideal-class)))

(defun fig//roll-character (name)
  "Roll a random character with NAME."
  (let* ((scores (fig//roll-ability-scores))
         (class (fig//pick-class scores)))
    (fig//make-rpg-character
     :name name
     :level 1
     :class class
     :max-hp (fig//roll (fig//rpg-class-hit-die class))
     :ability-scores scores)))

(defun fig//get-db-character (user)
  "Get the RPG character for USER."
  (alist-get :rpg-character (fig//load-db user)))

(defun fig//update-db-character (user f)
  "Apply F to to USER's saved RPG character.
If the user has no character, roll a new one."
  (fig//update-db-default user :rpg-character f (fig//roll-character user)))

(defun fig//character-to-string (char)
  "Convert CHAR to a human-readable description."
  (let ((scores (fig//rpg-character-ability-scores char)))
  (format
   "%s, the level %s %s | %s HP | STR %s DEX %s CON %s INT %s WIS %s CHA %s"
   (fig//rpg-character-name char)
   (fig//rpg-character-level char)
   (fig//rpg-class-name (fig//rpg-character-class char))
   (fig//rpg-character-max-hp char)
   (fig//rpg-ability-scores-str scores)
   (fig//rpg-ability-scores-dex scores)
   (fig//rpg-ability-scores-con scores)
   (fig//rpg-ability-scores-int scores)
   (fig//rpg-ability-scores-wis scores)
   (fig//rpg-ability-scores-cha scores))))

(provide 'fig-rpg)
;;; fig-rpg.el ends here
