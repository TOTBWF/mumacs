;;; tools/magit ---  -*- lexical-binding: t; -*-

;;; Commentary:

;; Configure `magit' and other git-related packages.

;;; Code:
(require 'core/meow)

;; Set up a `git' menu for `meow'.
(defconst meow-git-keymap (define-keymap))
(meow-define-keys 'leader `("g" . ("git" . ,meow-git-keymap)))

(use-package magit
  :commands magit-status magit-dispatch magit-file-dispatch
  :bind
  (:map magit-mode-map
	("x" . magit-discard))
  (:map meow-git-keymap
	("g" . magit-status)))

(use-package git-timemachine
  :commands git-timemachine
  :bind
  (:map meow-git-keymap
	("t" . git-timemachine))
  ;; `git-timemachine' fights with `meow' over keybindings,
  ;; so we replace all the problematic bindings.
  (:map git-timemachine-mode-map
	("N" . git-timemachine-show-next-revision)
	("P" . git-timemachine-show-previous-revision)))

(provide 'tools/magit)
;;; magit.el ends here
