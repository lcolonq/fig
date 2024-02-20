;;; fig-newspaper --- The Effort Post -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'dash)
(require 's)
(require 'f)
(require 'ht)
(require 'cl-lib)

(defvar fig//newspaper-todays-articles nil)

(cl-defstruct
    (fig//newspaper-article (:constructor fig//make-newspaper-article))
  headline
  author
  content)

(defun fig//newspaper-article-tex (a)
  "Convert an article A to TeX source."
  (s-concat
   "\\byline{" (fig//newspaper-article-headline a) "}{" (fig//newspaper-article-author a) "}\n"
   (fig//newspaper-article-content a)
   "\n\\closearticle\n"))

(cl-defstruct
    (fig//newspaper (:constructor fig//make-newspaper))
  slogan
  price
  articles)

(defun fig//newspaper-tex (np)
  "Convert a newspaper NP to TeX source."
  (s-replace-all
   (list
    (cons "FIG_SLOGAN" (fig//newspaper-slogan np))
    (cons "FIG_PRICE" (fig//newspaper-price np))
    (cons "FIG_ARTICLES" (apply #'s-concat (-map #'fig//newspaper-article-tex (fig//newspaper-articles np))))
    )
   (fig/slurp "~/src/fig/newspaper/template.tex")))

(defun fig//newspaper-pdf (src k)
  "Build TeX SRC to PDF.
Pass the path of the generated PDF to K."
  (let ((dir (make-temp-file "fig-newspaper" t))
        (srcfile (fig/tempfile "fig-newspaper-src" src ".tex")))
    (make-process
     :name "fig-newspaper-pdf"
     :buffer "*fig-newspaper-pdf*"
     ;; :command (list "xetex" "-interaction" "nonstopmode" "-jobname" "newspaper" "-output-directory" dir srcfile)
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
     :slogan "hello computer" :price "3 to 5"
     :articles
     fig//newspaper-todays-articles))
   #'find-file))

(provide 'fig-newspaper)
;;; fig-newspaper.el ends here
