;;; core/tweaks --- An assortment of tweaks to the core emacs experience  -*- lexical-binding: t; -*-

;;; Commentary:
;; This may seem a bit out of place, but we want to get some important
;; UI tweaking code loaded early in the init process so that we can
;; use it inside of things like `core/keys'.  Also included in this
;; file are UI tweaks that we want to apply ASAP, in case some other
;; part of the loading process fails.

;;; Code:
(require 'use-package)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Interface Tweaks

;; Disable the bell ring (Who thought this was a good idea!?!?)
(setq ring-bell-function 'ignore)

;; Make "yes or no" prompts use "y" and "n" instead.
(defalias 'yes-or-no-p 'y-or-n-p)

;; Of course we want this!
(setq enable-recursive-minibuffers t)

;; Start emacs maximized
(add-to-list 'default-frame-alist '(fullscreen . fullboth))

;; I'm fine with narrowing being enabled.
(put 'narrow-to-region 'disabled nil)

(provide 'core/tweaks)
;;; tweaks.el ends here
