;; -*- lexical-binding: t; byte-compile-warnings: nil  -*-
;; core
(add-to-list 'load-path (f-canonical "./"))
(require 'fig-utils)
(require 'fig-network)
(require 'fig-sensitive)
(require 'fig-db2)
(require 'fig-database)
(require 'fig-bullfrog)
(require 'fig-model)
(require 'fig-twitch)
(require 'fig-obs)
(require 'fig-chat)
(require 'fig)
;; gizmos
(add-to-list 'load-path (f-canonical "./gizmo/"))
(require 'fig-pronunciation)
(require 'fig-genealogy)
(require 'fig-hexamedia)
(require 'fig-uwoomfie)
(require 'fig-shindaggers)
(require 'fig-copfish)
(require 'fig-gcp)
(require 'fig-wikipedia)
(require 'fig-geiser)
(require 'fig-rpg)
(require 'fig-voice)
(require 'fig-emotes)
(require 'fig-bible)
(require 'fig-clone)
(require 'fig-fakechat)
(require 'fig-friend)
(require 'fig-newspaper)
(require 'fig-curse)
(require 'fig-bless)
;; contrib
(add-to-list 'load-path (f-canonical "./contrib/"))
(require 'bezelea-muzak)
;; run!
(fig/connect)

;; Local Variables:
;; flycheck-disabled-checkers: (emacs-lisp-checkdoc)
;; End:
