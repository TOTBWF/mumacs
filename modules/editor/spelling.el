;;; editor/spelling ---  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:
(require 'core/meow)

;; Set up a keymap for spelling-related keybindings,
;; place it at `C-#', and add it to the `meow' keypad.
(defconst meow-spelling-keymap (make-sparse-keymap)
  "Keymap for spelling-related keybindings.")

(keymap-global-set "C-#" meow-spelling-keymap)
(add-to-list 'meow-keypad-start-keys '(?# . ?#))

(use-package spell-fu
  :preface
  (declare-function spell-fu-dictionary-add "spell-fu.el")
  (declare-function spell-fu-get-personal-dictionary "spell-fu.el")
  (defun spell-fu-setup-dictionaries ()
    (spell-fu-dictionary-add (spell-fu-get-personal-dictionary "en-personal" "~/.aspell.en.pws")))
  :commands spell-fu-mode
  :hook (spell-fu-mode . spell-fu-setup-dictionaries)
  :bind
  (:map meow-spelling-keymap
	("a" . spell-fu-word-add)
	("d" . spell-fu-mode)))

(use-package ispell
  :straight nil
  :bind
  (:map meow-spelling-keymap
	("s" . ispell-word)))

(provide 'editor/spelling)
;;; spelling.el ends here

