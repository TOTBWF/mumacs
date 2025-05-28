;;; editor/auth-source.el ---  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:
(use-package auth-source
  :ensure nil
  :demand t
  :custom
  (auth-sources
   '(macos-keychain-internet
     macos-keychain-generic
     "~/.authinfo"
     "~/.authinfo.gpg")))

(provide 'editor/auth-source)
;;; auth-source.el ends here
