;;; editor/flycheck ---  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:
(require 'core/meow)

(use-package flycheck
  :ensure t
  :commands flycheck-mode
  :diminish flycheck-mode
  :custom
  ;; Use the `load-path' that we have already set up when checking
  ;; elisp.
  (flycheck-emacs-lisp-load-path 'inherit)
  (flycheck-keymap-prefix (kbd "C-!"))
  :config
  ;; Add `C-!' to the meow keypad translation; this avoids it showing up in `C-c'.
  (add-to-list 'meow-keypad-start-keys '(?! . ?!)))

(provide 'editor/flycheck)
;;; flycheck.el ends here
