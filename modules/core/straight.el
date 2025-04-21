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
(straight-use-package
 '(use-package
    :type git
    :host github
    :repo "jwiegley/use-package"))

(require 'use-package)

(defun use-package-normalize-advice (_name keyword args)
  ; checkdoc-params: (keyword)
  "Ensure ARGS is a valid advice form for the `use-package' `:advice' keyword.
See Info node `(use-package)Creating an extension'."
  (let ((arg args)
	args*)
    (while arg
      (let ((sym (caar arg))
	    (how (cadar arg))
	    (fn (caddar arg))
	    (props (car (cdddar arg))))
	(cond
	 ((and (symbolp sym) (assq how advice--how-alist) (use-package-recognize-function fn))
	  (setq args* (nconc args* (list (list sym how fn props)))) (setq arg (cdr arg)))
	 (t (use-package-error (concat (symbol-name keyword) " wants arguments acceptable to `advice-add'," " or a list of such values"))))))
    args*))

(defun use-package-autoloads-advice (_name _keyword args)
   ; checkdoc-params: (args)
  "Autoloads handler for the `use-package' `:advice' keyword.
See Info node `(use-package)Creating an extension'."
  (mapcar (lambda (arg) (cons (caddr arg) 'command)) args))

(defun use-package-handler-advice (name _keyword args rest state)
  ; checkdoc-params: (name args rest state)
  "Handler for the `use-package' `:advice' keyword.
See Info node `(use-package)Creating an extension'."
  (use-package-concat
   (use-package-process-keywords name rest state)
   `(,@(mapcar (pcase-lambda (`(,sym ,how ,fn ,props)) `(advice-add (quote ,sym) ,how (function ,fn) ,props)) args))))

(defalias 'use-package-normalize/:advice 'use-package-normalize-advice)
(defalias 'use-package-autoloads/:advice 'use-package-autoloads-advice)
(defalias 'use-package-handler/:advice 'use-package-handler-advice)

;; Make sure to place the `:advice' keywords before `:commands', as we are setting up autoloads.
(push :advice (nthcdr (use-package-keyword-index :commands) use-package-keywords))

;; This is handy for debugging.
(setopt use-package-compute-statistics t)

;; This makes it easier to track down deferal mistakes; if we need to
;; `:demand', then we should call it out!
(setopt use-package-always-defer t)

;; Don't add `-hook' implicitly to hook names inside of a `:hook' block.
(setopt use-package-hook-name-suffix nil)

;; We want to load this relatively early so that we can restart emacs nicely
;; even if we make a mistake in our config.
(use-package restart-emacs
  :demand t)

;; `diminish' lets us easily hide active modes from the modeline.
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
  :demand t)

;; `f.el' provides a more consistent API for filepath manipulation.
(use-package f
  :demand t)

;; `s.el' provides a more consistent API for string manipulation.
(use-package s
  :demand t)

(provide 'core/straight)
;;; straight.el ends here
