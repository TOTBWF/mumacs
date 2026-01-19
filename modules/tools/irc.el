;;; tools/irc --- IRC in Emacs -*- lexical-binding: t; -*-

;;; Commentary:
;; This module turns Emacs into an IRC client by installing
;; `circe'.
;;
;; All user-specific configuration should be done in the `custom-file'
;; via `circe-network-options'.

;;; Code:
(require 'core/elpaca)

(use-package circe
  :preface
  (defun circe-disable-font-locking ()
    "Disable font-locking when in `circe-mode'."
    (setq-local font-lock-ensure-function 'ignore)
    (font-lock-mode 0))
  :ensure t
  :commands circe
  :hook
  (circe-mode-hook . circe-disable-font-locking))

(provide 'tools/irc)
;;; irc.el ends here
