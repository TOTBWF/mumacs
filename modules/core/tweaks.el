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

;; If we want line-by-line scrolling, uncomment this line!
(setq scroll-conservatively 101)

;; However, this comes at a cost: we constantly have to redisplay,
;; which then means that scroll performance tanks. Therefore, I've
;; set this to 0 for now.
;; (setq scroll-conservatively 0)

(setq mouse-wheel-scroll-amount '(1))
(setq mouse-wheel-progressive-speed nil)
(setq redisplay-skip-fontification-on-input t)

;; Some fonts can cause a lot of allocations. If they are then
;; removed from the font cache, they need to be opened again
;; during redisplay, which can cause performance issues.
(setq inhibit-compacting-font-caches t)

(setq jit-lock-defer-time nil)
(setq jit-lock-chunk-size 5000)

;; This /drastically/ speeds up scrolling.
(setq truncate-lines t)

;; Avoid calling line-move-partial, increasing scroll speed.
;; https://emacs.stackexchange.com/questions/28736/emacs-pointcursor-movement-lag/28746
(setq auto-window-vscroll nil)

;; Disable the window decorations
(tool-bar-mode -1)
(scroll-bar-mode -1)
(menu-bar-mode -1)

;; Disable the bell ring (Who thought this was a good idea!?!?)
(setq ring-bell-function 'ignore)

;; Make "yes or no" prompts use "y" and "n" instead.
(defalias 'yes-or-no-p 'y-or-n-p)

;; Of course we want this!
(setq enable-recursive-minibuffers t)

(provide 'core/tweaks)
;;; tweaks.el ends here
