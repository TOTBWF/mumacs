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
  :custom
  ;; The default is `/usr/local/bin/git', which is a wrapper around the actual
  ;; git binary shipped by Xcode. This adds some noticable overhead, so we opt
  ;; to use the one shipped by Xcode directly.
  (magit-git-executable
   (let ((git-path "/Applications/Xcode.app/Contents/Developer/usr/bin/git"))
     (if (and (eq system-type 'darwin) (file-exists-p git-path))
	 git-path
       "/usr/local/bin/git")))
  :bind
  (:map magit-mode-map
	("x" . magit-discard))
  (:map meow-git-keymap
	("g" . magit-status)))

(use-package forge
  :after magit
  :demand t)

(use-package code-review
  :straight
  (code-review
   :fork "phelrine/code-review"
   :branch "fix/closql-update")
  :after magit forge
  :demand t
  :bind
  (:map code-review-mode-map
	("RET" . magit-diff-visit-file-other-window)
	("C-j" . code-review-comment-jump-next)
	("C-k" . code-review-comment-jump-previous)))

(use-package orgit
  :after magit org
  :demand t)

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
