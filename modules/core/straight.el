;;; core/straight --- Boostrapping code for straight.el  -*- lexical-binding: t; -*-

;;; Commentary:

;; Bootstrap `straight.el' and `use-package`, and configure some core
;; features that should happen early in the boostrapping process.

;;; Code:

;; Bootstrapping code for `straight.el'; taken from https://github.com/radian-software/straight.el.
(defvar bootstrap-version)
(defconst straight-use-package-by-default t)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(require 'straight)

;; Load up `use-package', so we can declaratively manage packages.
;; Note that we want to use HEAD for `use-package', as we want
;; `which-key' integration in the `:bind' form.
(straight-use-package '(use-package
			 :type git
			 :host github
			 :repo "jwiegley/use-package"))

(require 'use-package)

;; This is handy for debugging.
(setopt use-package-compute-statistics t)
;; This makes it easier to track down deferal mistakes; if we need to
;; `:demand', then we should call it out!
(setopt use-package-always-defer t)

;; We want to load this relatively early so that we can restart emacs nicely
;; even if we make a mistake in our config.
(use-package restart-emacs
  :demand t)

(use-package benchmark-init
  :disabled
  :straight (:build (:not compile))
  :demand t
  :functions benchmark-init/activate
  :config
  (benchmark-init/activate))

;; "diminish" lets us easily hide active modes from the modeline.
;; This seems like a bit of an odd place to load this, but
;; we use it absolutely everywhere, so we want it right
;; out the gate.
(use-package diminish
  :demand t
  :functions diminish
  :config
  (diminish 'global-whitespace-mode)
  (diminish 'auto-revert-mode)
  (diminish 'eldoc-mode)
  (diminish 'abbrev-mode))

;; We also want to configure native compilation to use a decent optimization level.
;; See (info "(elisp) Native Compilation") for more details.
(use-package comp
  :straight nil
  :demand t
  :if (native-comp-available-p)
  :custom
  (native-comp-speed 2 "Use the highest safe optimization level for native compilation."))

;;; Core packages:

;; These packages are extremely common, so we load them first ourselves
;; so we can use them too!

;; `dash.el' provides a more consistent API for manipulating lists; see (info "(dash) Top") for docs.
(use-package dash
  :defer nil)

;; `f.el' provides a more consistent API for filepath manipulation.
(use-package f
  :defer nil)

;; `s.el' provides a more consistent API for string manipulation.
(use-package s
  :defer nil)

(provide 'core/straight)
;;; straight.el ends here
