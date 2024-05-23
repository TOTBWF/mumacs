;;; tools/magit ---  -*- lexical-binding: t; -*-

;;; Commentary:

;; Configure `magit' and other git-related packages.

;;; Code:
(require 'core/meow)

(use-package magit
  :commands magit-status magit-dispatch magit-file-dispatch
  :bind
  (:map magit-mode-map
	("x" . magit-discard))
  (:map meow-leader-keymap
	("g" . magit-status)))

(use-package git-timemachine
  :commands git-timemachine
  :bind
  (:map meow-leader-keymap
	("G" . git-timemachine)))

(provide 'tools/magit)
;;; magit.el ends here
