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
  :ensure t
  :commands
  circe)

(provide 'tools/irc)
;;; irc.el ends here
