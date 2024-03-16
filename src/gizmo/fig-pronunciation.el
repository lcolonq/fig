;;; fig-pronunciation --- Geiser counter and factions -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'dash)
(require 's)
(require 'f)
(require 'ht)
(require 'cl-lib)

(defconst fig//pronunciation-premade ;; funny options
  '("LCOLONQ"
    "Joel"
    "mod clonk"
    "Columbo"
    "/ɛ:l.kʰɔloʊŋkʰ/"
    "Γ Column"
    "notgeiser"
    "funny magic man"
    "Lucius Coloncus Quintilianus"
    "rogueliTe"
    "Heidy Barnett"
    "Krya"
    "Laconic"
    "Loincloth"
    "Costco"
    ))

(defconst fig//pronunciation-part1 ;; the LLLL
  '("El"
    "Eel"
    "El El El El"
    "La"
    "Le"
    "Luh"
    "Loo"
    "Lo"
    "Al"
    "All"
    "Ale"
    "Ail"
    "Fifty"
    "Long"
    "Long Long Long Long"
    ))

(defconst fig//pronunciation-part2 ;; the Colon
  '("Colon"
    "Cologne"
    "Collin"
    "Clon"
    "Clown"
    "Clone"
    "Clun"
    "Cuhlun"
    "See"
    "Cloin"
    "Coloin"
    ))

(defconst fig//pronunciation-part3 ;; the Q
  '("Kuh"
    "Queue"
    "Kweh"
    "Kiu"
    "Kiew"
    "Coo"
    "Kewl"
    ))

(defun fig//pronuciation ()
  "Determine the canonical pronunciation of LCOLONQ."
  (if (= 0 (random 10))
      (fig/random-elt fig//pronunciation-premade)
    (let ((part1 (fig/random-elt fig//pronunciation-part1))
          (part2 (fig/random-elt fig//pronunciation-part2))
          (part3 (fig/random-elt fig//pronunciation-part3))
          (skip1 (= 0 (random 5)))
          (skip3 (= 0 (random 5)))
          (merge (= 0 (random 2))))
      (s-concat
       (if skip1 "" (s-concat part1 " "))
       part2
       (if skip3
           ""
         (if merge
             (s-downcase part3)
           (s-concat " " part3)))))))

(provide 'fig-pronunciation)
;;; fig-pronunciation.el ends here
