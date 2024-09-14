;;; early-init ---  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

;; We use `straight.el' for package management, so we disable
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

;;; early-init.el ends here
