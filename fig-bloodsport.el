;;; fig-bloodsport --- Game prototype -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'dash)
(require 's)
(require 'f)
(require 'ht)
(require 'cl-lib)

(defconst fig//bloodsport-move-dirs
  '((north . (0 . -1))
    (south . (0 . 1))
    (west . (-1 . 0))
    (east . (1 . 0))
    (northwest . (-1 . -1))
    (northeast . (1 . -1))
    (southwest . (-1 . 1))
    (southeast . (1 . 1))))

(defconst fig//bloodsport-map-w 4)
(defconst fig//bloodsport-map-h 4)
(setq fig//bloodsport-map (make-vector (* fig//bloodsport-map-w fig//bloodsport-map-h) nil))

(cl-defstruct
    (fig//bloodsport-slot
     (:constructor fig//make-bloodsport-slot))
  name
  used
  actions)

(cl-defstruct
    (fig//bloodsport-char
     (:constructor fig//make-bloodsport-char))
  name
  (symbol "@")
  facing
  (max-health 0)
  (health 0)
  temp-status
  slots)

(defun fig//bloodsport-coords-to-index (x y)
  "Convert X and Y to a map index."
  (+ x (* y fig//bloodsport-map-w)))

(defun fig//bloodsport-index-to-coords (i)
  "Convert map index I to coordinates."
  (cons
   (% i fig//bloodsport-map-w)
   (/ i fig//bloodsport-map-w)
   ))

(defun fig//bloodsport-render-map ()
  "Convert the map into a string."
  (apply #'s-concat
   (--map
    (s-concat
     (if (= 0 (% (cdr it) fig//bloodsport-map-w)) "\n")
     (if (car it) (fig//bloodsport-char-symbol (car it)) "."))
    (-zip-pair
     (seq-into fig//bloodsport-map 'list)
     (-iota (* fig//bloodsport-map-w fig//bloodsport-map-h))))))

(defun fig//bloodsport-map-get-char (name)
  "Get the character NAME."
   (car
    (--filter
     (and it (s-equals? (fig//bloodsport-char-name it) name))
     (seq-into fig//bloodsport-map 'list))))

(defun fig//bloodsport-map-get-char-index (name)
  "Get the map index for NAME."
  (cdr
   (car
    (--filter
     (and (car it) (s-equals? (fig//bloodsport-char-name (car it)) name))
     (-zip-pair
      (seq-into fig//bloodsport-map 'list)
      (-iota (* fig//bloodsport-map-w fig//bloodsport-map-h)))))))

(defun fig//bloodsport-map-get-char-coords (name)
  "Get the coordinates for NAME."
  (fig//bloodsport-index-to-coords (fig//bloodsport-map-get-char-index name)))

(defun fig//bloodsport-map-get (x y)
  "Get the map cell at X, Y."
  (aref fig//bloodsport-map (fig//bloodsport-coords-to-index x y)))

(defun fig//bloodsport-map-put (x y cell)
  "Set the map cell at X, Y to CELL."
  (setf
   (aref fig//bloodsport-map (fig//bloodsport-coords-to-index x y))
   cell))

(defun fig//bloodsport-map-move (x y dir)
  "Move the character at X, Y in DIR."
  (when-let ((char (fig//bloodsport-map-get x y))
             (offsets (alist-get dir fig//bloodsport-move-dirs)))
    (fig//bloodsport-map-put x y nil)
    (fig//bloodsport-map-put (+ x (car offsets)) (+ y (cdr offsets)) char)))

(defun fig//bloodsport-move-char (name dir)
  "Move the character NAME in DIR."
  (when-let ((coords (fig//bloodsport-map-get-char-coords name)))
    (fig//bloodsport-map-move (car coords) (cdr coords) dir)))

(defun fig//bloodsport-make-test-char1 ()
  "Create first test character."
  (fig//bloodsport-map-put
   1 1
   (fig//make-bloodsport-char
    :name "Foo"
    :symbol "@"
    :facing 'south
    :health 5
    :max-health 5
    :slots
    (list
     (fig//make-bloodsport-slot :name "Head" :used nil :actions '(headbutt))
     (fig//make-bloodsport-slot :name "LeftArm" :used nil :actions '(sword))
     (fig//make-bloodsport-slot :name "RightArm" :used nil :actions '(shield))
     (fig//make-bloodsport-slot :name "Legs" :used nil :actions '(north south west east))))))

(defun fig//bloodsport-make-test-char2 ()
  "Create second test character."
  (fig//bloodsport-map-put
   3 1
   (fig//make-bloodsport-char
    :name "Slime"
    :symbol "S"
    :facing 'south
    :health 5
    :max-health 5
    :slots
    (list))))
(fig//bloodsport-make-test-char1)
(fig//bloodsport-make-test-char2)

(defun fig//bloodsport-left-hand-shield ()
  "Activate shield for test player."
  (let ((char (fig//bloodsport-map-get-char "Foo")))
    (setf
     (fig//bloodsport-char-temp-status char)
     (cons
      'shield
      (fig//bloodsport-char-temp-status char)))))

(defun fig//bloodsport-hit-char (char dir)
  "Hit CHAR once from DIR."
  (ignore dir)
  (if (-contains? (fig//bloodsport-char-temp-status char) 'shield)
      (delete 'shield (fig//bloodsport-char-temp-status char))
    (cl-decf (fig//bloodsport-char-health char))))

(defun fig//bloodsport-end-turn ()
  "Run the enemy turn and reset slots."
  (let* ((cur (fig//bloodsport-map-get-char-coords "Slime"))
         (player (fig//bloodsport-map-get-char-coords "Foo"))
         (adjacent (= 1 (+ (abs (- (car cur) (car player))) (abs (- (cdr cur) (cdr player))))))
         (dir
          (cond
           ((> (car cur) (car player)) 'west)
           ((< (car cur) (car player)) 'east)
           ((> (cdr cur) (cdr player)) 'north)
           ((< (cdr cur) (cdr player)) 'south)
           (t 'north))))
    (if adjacent
        (fig//bloodsport-hit-char (fig//bloodsport-map-get-char "Foo") dir)
      (fig//bloodsport-move-char "Slime" dir))))

(provide 'fig-bloodsport)
;;; fig-bloodsport.el ends here
