;;; tools/proced ---  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:
(use-package proced
  :elpaca nil
  :commands
  proced
  :custom
  (proced-auto-update-flag t)
  (proced-enable-color-flag t)
  (proced-format 'custom)
  :config
  (add-to-list
   'proced-format-alist
   '(custom user pid pcpu comm)))

(provide 'tools/proced)
;;; proced.el ends here
