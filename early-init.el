;;; early-init ---  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

;; We use `elpaca.el' for package management, so we disable
;; `package.el' at startup to avoid loading packages twice.
(setq package-enable-at-startup nil)

;; By default, the GC threshold for emacs is 800Kib, which is a bit low for initialization.
;; To avoid GC while we initialize, let's bump it up to 10Mib.
(setq gc-cons-threshold 10000000)

;; Restore the GC threshold after initialization is complete.
(add-hook 'after-init-hook
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

;; (unless (memq 'menu-bar minimal-emacs-ui-features)
;;   (push '(menu-bar-lines . 0) default-frame-alist)
;;   (unless (memq window-system '(mac ns))
;;     (setq menu-bar-mode nil)))
;;; early-init.el ends here
