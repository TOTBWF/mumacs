;;; core/basics --- Boostrapping code for straight.el  -*- lexical-binding: t; -*-

;;; Commentary:

;; Configure some core features that should happen early in the boostrapping
;; process.

;;; Code:

(require 'core/elpaca)

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
  :ensure t
  :demand t)

;; `diminish' lets us easily hide active modes from the modeline.
;; This seems like a bit of an odd place to load this, but
;; we use it absolutely everywhere, so we want it right
;; out the gate.
(use-package diminish
  :ensure t
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
  :ensure nil
  :demand t
  :if (native-comp-available-p)
  :custom
  (native-comp-speed 2 "Use the highest safe optimization level for native compilation."))

;;; Core packages:

;; These packages are extremely common, so we load them first ourselves
;; so we can use them too!

;; `dash.el' provides a more consistent API for manipulating lists; see (info "(dash) Top") for docs.
(use-package dash
  :ensure t
  :demand t)

;; `f.el' provides a more consistent API for filepath manipulation.
(use-package f
  :ensure t
  :demand t)

;; `s.el' provides a more consistent API for string manipulation.
(use-package s
  :ensure t
  :demand t)

(provide 'core/basics)
;;; basics.el ends here
