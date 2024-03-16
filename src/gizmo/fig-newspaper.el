;;; fig-newspaper --- The Effort Post -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'dash)
(require 's)
(require 'f)
(require 'ht)
(require 'cl-lib)

(defvar fig//newspaper-todays-articles nil)

(defconst fig//newspaper-slogans
  (list
   "hello computer"
   "only on !discord IRC"
   "GoMoCo HaThPl"
   "good morning computer"
   "hack the planet"
   "!oomfie"
   "All the news that's fit to prin1"
   "I use arch by the way"
   "play void stranger (2023)"
   "[i](this was sent from godot)[i]"
   "LCOLONQ Lies in LaTeX"
   "Super idol's smile / Is not as sweet as yours / The sunlight at noon in August / Does not shine like you / Love the 105 °C you / Distilled water that is pure every drop"
   "this is where we read about the computer"
   "brought to you by viewers like you. thank you!"
   ))

(defconst fig//newspaper-prices
  (list
   "1 COLON"
   "3 to 5"
   "501 Internal Server Error"
   "$3.50"
   "206 bpm"
   "1 boost"
   "a snack for friend"
   "59 frames per second"))

(cl-defstruct
    (fig//newspaper-article (:constructor fig//make-newspaper-article))
  headline
  author
  content)

(defun fig//newspaper-wrap-emoji (s)
  "Wrap emoji with appropriate TeX in S."
  (s-replace-regexp "[^[:ascii:]]" (lambda (c) (format "{\\\\figemote %s}" c)) s))

(defun fig//newspaper-escape (s)
  "Apply appropriate subsitutions to S."
  (s-replace-regexp
   (rx "\"" (one-or-more (not "\"")) "\"")
   (lambda (x)
     (s-concat "``" (s-chop-suffix "\"" (s-chop-prefix "\"" x)) "''"))
   (s-replace-all
    '(("&" . "\\&")
      ("%" . "\\%")
      ("$" . "\\$")
      ("#" . "\\#")
      ("_" . "\\_")
      ("{" . "\\{")
      ("}" . "\\}")
      ("~" . "\\textasciitilde")
      ("^" . "\\textasciicircum")
      ("\\" . "\\textbackslash"))
    s)
   nil
   t))

(defun fig//newspaper-article-tex (a)
  "Convert an article A to TeX source."
  (s-concat
   "\\byline{"
   (fig//newspaper-wrap-emoji (fig//newspaper-escape (fig//newspaper-article-headline a)))
   "}{"
   (fig//newspaper-wrap-emoji (fig//newspaper-escape (fig//newspaper-article-author a)))
   "}\n"
   (fig//newspaper-wrap-emoji (fig//newspaper-escape (fig//newspaper-article-content a)))
   "\n\\closearticle\n"))

(cl-defstruct
    (fig//newspaper (:constructor fig//make-newspaper))
  slogan
  price
  articles
  (edition 1))

(defun fig//newspaper-tex (np)
  "Convert a newspaper NP to TeX source."
  (s-replace-all
   (list
    (cons "FIG_EDITION" (number-to-string (fig//newspaper-edition np)))
    (cons "FIG_SLOGAN" (fig//newspaper-slogan np))
    (cons "FIG_PRICE" (fig//newspaper-price np))
    (cons "FIG_ARTICLES" (apply #'s-concat (-map #'fig//newspaper-article-tex (fig//newspaper-articles np))))
    )
   (fig/slurp "~/src/fig/assets/newspaper/template.tex")))

(defun fig//newspaper-pdf (src k)
  "Build TeX SRC to PDF.
Pass the path of the generated PDF to K."
  (when (get-buffer "*fig-newspaper-pdf*")
    (with-current-buffer "*fig-newspaper-pdf*"
      (erase-buffer)))
  (let ((dir (make-temp-file "fig-newspaper" t))
        (srcfile (fig/tempfile "fig-newspaper-src" src ".tex")))
    (make-process
     :name "fig-newspaper-pdf"
     :buffer "*fig-newspaper-pdf*"
     :command (list "print-newspaper" srcfile dir)
     :sentinel
     (lambda (_ _)
       (funcall k (f-join dir "newspaper.pdf"))))))

(defvar fig//newspaper-test-issue
  (fig//make-newspaper
   :slogan "hello computer" :price "3 to 5"
   :articles
   (list
    (fig//make-newspaper-article
     :headline "omg hi oomfie"
     :author "Joel"
     :content "\\lipsum[1]")
    (fig//make-newspaper-article
     :headline "omg hi oomfie"
     :author "Joel"
     :content "\\lipsum[1]")
    (fig//make-newspaper-article
     :headline "omg hi oomfie"
     :author "Joel"
     :content "\\lipsum[1]")
    (fig//make-newspaper-article
     :headline "omg hi oomfie"
     :author "Joel"
     :content "\\lipsum[1]")
    )))

(defun fig/newspaper ()
  "Generate and open today's work-in-progress newspaper."
  (interactive)
  (fig//newspaper-pdf
   (fig//newspaper-tex
    (fig//make-newspaper
     :slogan (fig/random-elt fig//newspaper-slogans) :price (fig/random-elt fig//newspaper-prices)
     :edition (fig//load-db-entry "LCOLONQ" :news-edition)
     :articles
     fig//newspaper-todays-articles))
   #'find-file))

(defun fig/newspaper-publish ()
  "Finalize and publish today's work-in-progress newspaper."
  (interactive)
  (let ((edition (fig//load-db-entry "LCOLONQ" :news-edition)))
    (fig//newspaper-pdf
     (fig//newspaper-tex
      (fig//make-newspaper
       :slogan (fig/random-elt fig//newspaper-slogans) :price (fig/random-elt fig//newspaper-prices)
       :edition edition
       :articles
       fig//newspaper-todays-articles))
     (lambda (path)
       (make-process
        :name "fig-newspaper-publish"
        :command (list "scp" path (format "llll@pub.colonq.computer:~/public_html/news/%03d.pdf" edition))
        :sentinel
        (lambda (_ _)
          (fig//update-db-number "LCOLONQ" :news-edition #'1+)
          (browse-url (format "https://pub.colonq.computer/~llll/news/%03d.pdf" edition))
          ))))))

(provide 'fig-newspaper)
;;; fig-newspaper.el ends here
