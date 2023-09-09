;;; fig-breed --- Need for breed -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'dash)
(require 's)
(require 'f)
(require 'ht)
(require 'color)
(require 'cl-lib)

(cl-defstruct
    (fig//palette
     (:constructor fig//make-palette))
  pedigree
  usage
  name
  hair
  eyes
  highlight
  skin)

(defun fig//read-palette (p)
  "Given a palette expression P, convert to a `fig//palette' struct."
  (fig//make-palette
   :pedigree (nth 0 p)
   :usage (nth 1 p)
   :name (nth 2 p)
   :hair (nth 3 p)
   :eyes (nth 4 p)
   :highlight (nth 4 p)
   :skin (nth 5 p)))

(defun fig//write-palette (p)
  "Given a palette P, convert to a palette expression."
   (list
    (fig//palette-pedigree p)
    (fig//palette-usage p)
    (fig//palette-name p)
    (fig//palette-hair p)
    (fig//palette-eyes p)
    (fig//palette-highlight p)
    (fig//palette-skin p)))

(defvar fig/palettes nil)
(defun fig//save-palettes ()
  "Save the quotes database."
  (fig//save-db "__PALETTES__" (-map #'fig//write-palette fig/palettes)))
(defun fig//load-palettes ()
  "Load the quotes database."
  (setf fig/palettes (-map #'fig//read-palette (fig//load-db "__PALETTES__"))))
(defun fig//get-palette (name)
  "Retrieve the palette named NAME."
  (--find (s-equals? name (fig//palette-name it)) fig/palettes))
(defun fig//add-palette (name hair eyes highlight)
  "Add palette named NAME with colors for HAIR EYES and HIGHLIGHT."
  (unless (fig//get-palette name)
    (add-to-list
     'fig/palettes
     (fig//make-palette
      :name name
      :hair hair
      :eyes eyes
      :highlight highlight)))
  (fig//save-palettes))
(fig//load-palettes)

(defun fig//model-use-palette (pal)
  "Use the palette PAL."
  (fig//model-palette "hair" "lcolonq" (fig//palette-hair pal))
  (fig//model-palette "eyes" "lcolonq" (fig//palette-eyes pal))
  (fig//model-palette "highlight" "lcolonq" (fig//palette-highlight pal)))
(defun fig//model-use-palette-preset (name)
  "Use the palette preset named NAME."
  (when-let ((pal (fig//get-palette name)))
    (setf
     (fig//palette-usage pal)
     (+ 1 (or (fig//palette-usage pal) 0)))
    (fig//save-palettes)
    (fig//model-use-palette pal)))

(defun fig//color16-to-color8 (c16)
  "Convert C16 to a single-byte color component."
  (/ c16 256))

(defun fig//color16-to-float (c16)
  "Convert C16 to a floating point color component."
  (/ c16 65535.0))

(defun fig//float-to-color8 (c)
  "Convert color float C to a single-byte color component."
  (truncate (* (color-clamp c) 255)))

(defun fig//color8-to-float (c8)
  "Convert C8 to a floating point color component."
  (/ c8 255.0))

(defun fig//color-to-genotype (color)
  "Convert COLOR to a genotype number."
  (let* ((vals (color-values color))
         (red (fig//color16-to-float (car vals)))
         (green (fig//color16-to-float (cadr vals)))
         (blue (fig//color16-to-float (caddr vals)))
         (hsl (color-rgb-to-hsl red green blue))
         (hue (fig//float-to-color8 (car hsl)))
         (sat (fig//float-to-color8 (cadr hsl)))
         (lig (fig//float-to-color8 (caddr hsl))))
    (logior
     (ash hue 16)
     (ash sat 8)
     lig)))

(defun fig//genotype-to-color (geno)
  "Convert GENO to a color string."
  (let* ((hue (fig//color8-to-float (ash geno -16)))
         (sat (fig//color8-to-float (logand #xff (ash geno -8))))
         (lig (fig//color8-to-float (logand #xff geno)))
         (rgb (color-hsl-to-rgb hue sat lig))
         (red (fig//float-to-color8 (car rgb)))
         (green (fig//float-to-color8 (cadr rgb)))
         (blue (fig//float-to-color8 (caddr rgb))))
    (format
     "#%02x%02x%02x"
     red green blue)))

(defvar fig/breeding-schemes
  (list
   ;; #'logior
   ;; #'logand
   (lambda (g1 g2)
     (when (and g1 g2)
       (/ (+ g1 g2) 2)))
   )
  "Available functions for producing child genotypes.")

(defun fig//breed-colors (c1 c2 scheme)
  "Breed colors C1 and C2 using SCHEME."
  (when (and c1 c2)
    (funcall scheme c1 c2)))

(defun fig//breed-names (n1 n2 k)
  "Breed names N1 and N2, passing the new name to K."
  (fig/ask
   (format "%s %s" n1 n2)
   (lambda (r)
     (funcall k (s-trim r)))
   "Given the names of two colorschemes, please respond with a new name that is a combination of the two. This name should be one single word."
   "FRENCH miku"
   "Friku"
  ))

(defun fig//breed-palettes (p1 p2 k)
  "Breed palettes P1 and P2, passing the new palette to K."
  (let* ((u1 (fig//palette-usage p1))
         (u2 (fig//palette-usage p2))
         (utotal (max 1 (+ u1 u2)))
         (scheme
          (lambda (c1 c2)
            (when (and c1 c2)
              (let ((g1 (fig//color-to-genotype c1))
                    (g2 (fig//color-to-genotype c2)))
                (fig//genotype-to-color
                 (truncate (* (+ g1 g2) (/ (* 1.0 u1) utotal)))
                 )))))
         (hair (fig//breed-colors (fig//palette-hair p1) (fig//palette-hair p2) scheme))
         (eyes (fig//breed-colors (fig//palette-eyes p1) (fig//palette-eyes p2) scheme))
         (highlight (fig//breed-colors (fig//palette-highlight p1) (fig//palette-highlight p2) scheme)))
    (fig//breed-names
     (fig//palette-name p1)
     (fig//palette-name p2)
     (lambda (nm)
       (funcall
        k
        (fig//make-palette
         :pedigree
         (cons
          (cons (fig//palette-name p1) (fig//palette-pedigree p1))
          (cons (fig//palette-name p2) (fig//palette-pedigree p2)))
         :name nm
         :hair hair
         :eyes eyes
         :highlight highlight
         ))))))
  
(provide 'fig-breed)
;;; fig-breed.el ends here
