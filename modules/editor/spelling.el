;;; editor/spelling ---  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:
(require 'core/meow)

(define-keypad meow-spelling-map ?#
  "Keymap for spelling-related keybindings.")

(use-package spell-fu
  :ensure t
  :preface





  ;; Helper for setting up dictionaries.
  (defun spell-fu-setup-dictionaries ()
    (spell-fu-dictionary-add (spell-fu-get-personal-dictionary "en-personal" "~/.aspell.en.pws")))
  :functions
  spell-fu-dictionary-add
  spell-fu-get-personal-dictionary
  :commands spell-fu-mode spell-fu-word-add
  :hook (spell-fu-mode-hook . spell-fu-setup-dictionaries)
  :bind
  (:map meow-spelling-map
	("a" . spell-fu-word-add)
	("d" . spell-fu-mode)))

;; Set up our `use-package' integration for `spell-fu'.
(defun use-package-autoloads-spell-fu (_name _keyword _args)
  ;; checkdoc-params: (args)
  "Autoloads handler for the `use-package' `:spell-fu' keyword.
See Info node `(use-package)Creating an extension'."
  nil)

(defun use-package-handler-spell-fu (name _keyword args rest state)
  ;; checkdoc-params: (name args rest state)
  "Handler for the `use-package' `:spell-fu' keyword.
See Info node `(use-package)Creating an extension'.

HACK: This function will add a lambda as a hook: this can result in
some surprising behaviour when re-running `use-package' blocks, as the
previous hook will not get removed.  To work around this, we add our
hook with depth -1, but this is a hack: we really should be creating a
`defun' instead."
  (use-package-concat
   (use-package-process-keywords name rest state)
   (if (and (boundp 'spell-fu-faces-include) (boundp 'spell-fu-faces-exclude))
       (mapcar
	(pcase-lambda (`(,hook ,sym . ,faces))
	  `(add-hook
	    (quote ,(intern (concat (symbol-name hook) use-package-hook-name-suffix)))
	    (lambda ()
	      (setq ,sym (quote ,faces))
	      (spell-fu-mode 1))
	    -1))
	args)
     (warn "use-package-handler-spell-fu: `spell-fu' face include/exclude lists not bound."))))

(defun use-package-normalize-spell-fu (_name keyword args)
  ;; checkdoc-params: (keyword)
  "Ensure ARGS is a valid advice form for the `use-package' `:spell-fu' keyword.
See Info node `(use-package)Creating an extension'."
  (mapcar
   (lambda (arg)
     (pcase arg
       (`(,hook :exclude . ,faces)
	`(,hook spell-fu-faces-exclude . ,faces))
       (`(,hook :include . ,faces)
	`(,hook spell-fu-faces-include . ,faces))
       (_
	(use-package-error
	 (concat (symbol-name keyword) " expects either a single (:exclude FACES) or (:include FACES) form.")))))
   args))

;; The `:spell-fu' keyword is a glorified version of `:hook', so we place the
;; keyword right before `:hook'.
(defalias 'use-package-normalize/:spell-fu 'use-package-normalize-spell-fu)
(defalias 'use-package-handler/:spell-fu 'use-package-handler-spell-fu)
(push :spell-fu (nthcdr (use-package-keyword-index :hook) use-package-keywords))

(use-package ispell
  :ensure nil
  :bind
  (:map meow-spelling-map
	("s" . ispell-word)))

(provide 'editor/spelling)
;;; spelling.el ends here
