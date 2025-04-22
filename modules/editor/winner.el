;;; editor/winner ---  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(use-package winner
  :straight nil
  :demand t
  :hook
  (after-init-hook . winner-mode))

(provide 'editor/winner)
;;; winner.el ends here
