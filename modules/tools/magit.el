;;; tools/magit ---  -*- lexical-binding: t; -*-

;;; Commentary:

;; Configure `magit' and other git-related packages.

;;; Code:
(require 'core/meow)
(require 'tools/org)

(define-leader meow-leader-git-map "g" "git"
  "Leader keymap for git-related keybindings.")

(use-package magit
  :ensure t
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
  (:map meow-leader-git-map
	("g" . magit-status)))

(use-package forge
  :ensure t
  :after magit)

(use-package orgit
  :ensure t
  :after (magit org))

(use-package orgit-forge
  :ensure t
  :after (forge magit org orgit))

(use-package git-timemachine
  :after (meow)
  :ensure t
  :commands git-timemachine
  :bind
  (:map meow-leader-git-map
	("t" . git-timemachine))
  ;; `git-timemachine' fights with `meow' over keybindings,
  ;; so we replace all the problematic bindings.
  (:map git-timemachine-mode-map
	("N" . git-timemachine-show-next-revision)
	("P" . git-timemachine-show-previous-revision)))

(provide 'tools/magit)
;;; magit.el ends here
