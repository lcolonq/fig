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

(provide 'fig-gamba)
;;; fig-gamba.el ends here
