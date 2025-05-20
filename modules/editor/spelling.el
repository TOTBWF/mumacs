;;; editor/spelling ---  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:
(require 'core/meow)

;; Set up a keymap for spelling-related keybindings,
;; place it at `C-#', and add it to the `meow' keypad.
(defconst meow-spelling-keymap (make-sparse-keymap)
  "Keymap for spelling-related keybindings.")

(with-eval-after-load 'meow
  (keymap-global-set "C-#" meow-spelling-keymap)
  (add-to-list 'meow-keypad-start-keys '(?# . ?#)))

(use-package spell-fu
  :ensure t
  :functions
  spell-fu-dictionary-add
  spell-fu-get-personal-dictionary
  :preface
  (defun spell-fu-setup-dictionaries ()
    (spell-fu-dictionary-add (spell-fu-get-personal-dictionary "en-personal" "~/.aspell.en.pws")))
  :commands spell-fu-mode spell-fu-word-add
  :hook (spell-fu-mode-hook . spell-fu-setup-dictionaries)
  :bind
  (:map meow-spelling-keymap
	("a" . spell-fu-word-add)
	("d" . spell-fu-mode)))

(defun use-package-normalize-spell-fu (_name keyword args)
  ; checkdoc-params: (keyword)
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


(defun use-package-autoloads-spell-fu (_name _keyword _args)
  ; checkdoc-params: (args)
  "Autoloads handler for the `use-package' `:spell-fu' keyword.
See Info node `(use-package)Creating an extension'."
  nil)


(defun use-package-handler-spell-fu (name _keyword args rest state)
  ; checkdoc-params: (name args rest state)
  "Handler for the `use-package' `:spell-fu' keyword.
See Info node `(use-package)Creating an extension'."
  (use-package-concat
   (use-package-process-keywords name rest state)
   (mapcar
    (pcase-lambda (`(,hook ,sym . ,faces))
      `(add-hook
	(quote ,(intern (concat (symbol-name hook) use-package-hook-name-suffix)))
	(lambda ()
	  (setq ,sym (quote ,faces))
	  (spell-fu-mode 1))))
    args)))


(defalias 'use-package-normalize/:spell-fu 'use-package-normalize-spell-fu)
(defalias 'use-package-handler/:spell-fu 'use-package-handler-spell-fu)

;; The `:spell-fu' keyword is a glorified version of `:hook', so we place the
;; keyword right before `:hook'.
(push :spell-fu (nthcdr (use-package-keyword-index :hook) use-package-keywords))

(use-package ispell
  :ensure nil
  :bind
  (:map meow-spelling-keymap
	("s" . ispell-word)))

(provide 'editor/spelling)
;;; spelling.el ends here
