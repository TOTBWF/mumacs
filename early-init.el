;;; early-init --- Early initialization -*- lexical-binding: t; -*-

;;; Commentary:
;; As of Emacs 27.1, the `early-init-file' is loaded before `package.el' and
;; the GUI is initialized.  This allows us to disable a bunch of superfluous
;; GUI features before they actually get loaded, which helps load performance.
;;
;; One should be very careful when modifying this file: this happens extremely
;; early in the load process, so any mistakes will result in a completely broken
;; Emacs configuration.  When in doubt, use `restart-emacs-start-new-emacs' to
;; test your changes.
;;
;; See [[info:emacs#Early Init File]] for further documentation on early initialization.

;;; Code:

;; We use `elpaca.el' for package management, so we disable
;; `package.el' at startup to avoid loading packages twice.
(setq package-enable-at-startup nil)

;; By default, the GC threshold for emacs is 800Kib, which is a bit low for initialization.
;; To avoid GC while we initialize, let's bump it up to 10Mib.
(setq gc-cons-threshold 10000000)

;; By default, emacs can only read 4k chunks from a process.
;; This causes most processes to run signifigantly slower when
;; run from emacs, as they spend most of their time waiting for
;; emacs to read input!
(setq read-process-output-max (* 64 1024 1024))

;; Restore the GC threshold after initialization is complete.
;; Note that we use `elpaca-after-init-hook' in place of `after-init-hook':
;; most of our time will be spent actually crunching through the `elpaca'
;; queues.
(add-hook 'elpaca-after-init-hook
          (lambda ()
            (setq gc-cons-threshold 1000000)
            (message "gc-cons-threshold restored to %S"
                     gc-cons-threshold)))

;; We need to remove the built-in `org-mode' from the `load-path' ASAP, before it is able
;; to litter autoloads all over the place that will cause us headaches.
;; Note that we can't use any slick functional programming here; we need to stick
;; to functions defined in `subr', as we are super early in the load process!
(setq load-path
      (let (result)
	(dolist (path load-path (nreverse result))
	  (unless (string-match-p "org$" path)
	    (push path result)))))

;; Add our modules to the load path.
(add-to-list 'load-path (file-name-concat user-emacs-directory "modules"))

;; We also make sure to set `custom-file' ASAP. This lets us control some
;; parts of the load process early.
(setq custom-file (file-name-concat user-emacs-directory "custom.el"))

;; Disable superfluous UI decorations.
;; We could call `menu-bar-mode', `tool-bar-mode', and `scroll-bar-mode',
;; but these all incur a somewhat costly frame redraw on startup.
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)
(push '(horizontal-scroll-bars) default-frame-alist)

(setq tool-bar-mode nil)
(setq menu-bar-mode nil)
(setq scroll-bar-mode nil)

;; Some fonts can cause a lot of allocations. If they are then
;; removed from the font cache, they need to be opened again
;; during redisplay, which can cause performance issues.
(setq inhibit-compacting-font-caches t)

;; Allow line-by-line scrolling
(setq scroll-conservatively 101)
(setq mouse-wheel-scroll-amount '(1))
(setq mouse-wheel-progressive-speed nil)

;; However, this comes at a cost: we constantly have to redisplay,
;; which then means that scroll performance tanks.
;; The following speeds up scroll performance somewhat.
(setq redisplay-skip-fontification-on-input t)

;; This /drastically/ speeds up scrolling.
(setq truncate-lines t)

;; Allow emacs to font-lock larger chunks. This can marginally
;; improve scrolling performance.
(setq jit-lock-defer-time nil)
(setq jit-lock-chunk-size 5000)

;; Avoid calling line-move-partial, increasing scroll speed.
;; https://emacs.stackexchange.com/questions/28736/emacs-pointcursor-movement-lag/28746
(setq auto-window-vscroll nil)

;; Disable bidirectional text scanning and the bidirectional parentheses algorithm.
;; This gives a modest performance increase.
(setq-default bidi-display-reordering 'left-to-right
              bidi-paragraph-direction 'left-to-right)
(setq bidi-inhibit-bpa t)

;; Don't run through the `auto-mode-alist' twice when trying
;; to determine the mode.
(setq auto-mode-case-fold nil)

;;; early-init.el ends here
