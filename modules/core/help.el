;;; core/help ---  -*- lexical-binding: t; -*-

;;; Commentary:
;; Customizations to Emacs's help system, and
;; tweaks to `info'.

;;; Code:
(require 'core/selectrum)

;; `iman' merges `man' and `info', and also stores an index.
(use-package iman
  :custom
  (iman-Man-index-command-and-args '("man" "-k" ".") "MacOS man does not list all man pages when passed ''.")
  :bind ("C-h C-i" . iman))

(use-package man
  :after selectrum
  :straight nil
  :bind
  (:map Man-mode-map
	("/" . ctrlf-forward-literal)))

;; `helpful' provides a nicer help menu.
(use-package helpful
  :after meow
  :defer t
  :init
  ;; All of our elisp files are source controlled thanks to `straight',
  ;; so we need to set this to be able to easily navigate to them inside
  ;; of help buffers.
  (setq vc-follow-symlinks t)
  (setq describe-bindings-outline t)
  ;; Banish the annoying `HELLO' file!
  (global-unset-key (kbd "C-h h"))
  ;; Do not need `view-emacs-FAQ'.
  (global-unset-key (kbd "C-h C-f"))
  :bind
  (("C-h f" . helpful-callable)
   ("C-h F" . describe-face)
   ("C-h v" . helpful-variable)
   ("C-h k" . helpful-key)
   ("C-h x" . helpful-command)
   ("C-h '" . describe-char)))

;; Demos of common elisp functions.
(use-package elisp-demos
  :after helpful
  :defer t
  :init
  (advice-add 'helpful-update :after 'elisp-demos-advice-helpful-update))

;; Override some annoying keybindings in `Info-mode'.
(use-package info
  :straight nil
  :bind
  (:map Info-mode-map
	("l" . meow-right)
	("h" . meow-left)))

(provide 'core/help)
;;; help.el ends here
