;;; tools/org ---  -*- lexical-binding: t; -*-

;;; Commentary:
;; We really want to use a version of `org-mode' provided via `straight',
;; so we make sure to strip out all occurances of the built-in `org' from the
;; load path.  This will prevent any annoying mistakes when calling `require'
;; on org-related files.  However, this needs to be done *extremely* early
;; in the load process; see `early-init.el' for the code that handles this.

;;; Code:
(require 'core/meow)

;; Set up a `org' menu for `meow'.
(defconst meow-org-keymap (define-keymap))
(meow-define-keys 'leader `("o" . ("org" . ,meow-org-keymap)))

;; We ensure that `org' is handled via `straight' to make `org-roam' happy.
(use-package org
  :custom
  (org-id-link-to-org-use-id 'create-if-interactive)
  (org-todo-keywords
   '((sequence "TODO(t)" "OPEN(o)" "|" "DONE(d)" "STOP(s")))
  (org-todo-keyword-faces
   '(("OPEN" . font-lock-constant-face)
     ("STOP" . font-lock-comment-face))))

(use-package ol-man
  :straight nil
  :after org)

;; Zotero link integration
(use-package zotxt
  :diminish org-zot-mode
  :hook (org-mode . org-zotxt-mode))

(use-package org-roam
  :after org
  :functions
  org-roam-db-autosync-mode
  :config
  ;; Keep the `org-roam' session synchronized.
  (org-roam-db-autosync-mode 1)
  :custom
  ;; Make sure that `org-roam' uses the same directory as `logseq'.
  (org-roam-directory "~/Documents/Notes")
  (org-roam-dailies-directory "journals/")
  (org-roam-capture-templates
   '(("n" "note" plain "%?" :target
      (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n#+category: note\n* ${title}\n")
      :unnarrowed t)
     ("t" "task" plain "%?" :target
      (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n#+category: task\n* TODO [#B] ${title}")
      :unnarrowed t)
     ("e" "event" plain "%?" :target
      (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n#+category: event\n* ${title}\nSCHEDULED: %^t")
      :unnarrowed t)
     ("p" "person" plain "%?" :target
      (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n#+category: person\n* ${title}")
      :unnarrowed t)))
  ;; HACK: This will get quite slow after a while; should use something smarter as detailed
  ;; in https://d12frosted.io/posts/2021-01-16-task-management-with-roam-vol5.html
  (org-agenda-files '("~/Documents/Notes/"))

  :bind
  (:map meow-org-keymap
	("a" . org-agenda)
	("o" . org-roam-node-find)
	("c" . org-roam-capture)
	("l" . org-store-link)
	("i" . org-roam-node-insert)
	("d" . org-roam-dailies-goto-today)
	("y" . org-roam-dailies-goto-yesterday)))

(use-package org-fancy-priorities
  :after org
  :diminish org-fancy-priorities-mode
  :hook
  (org-mode . org-fancy-priorities-mode)
  :custom
  (org-highest-priority ?A)
  (org-default-priority ?B)
  (org-lowest-priority ?D)
  (org-fancy-priorities-list
   '((?A . "❗")
     (?B . "⚠️")
     (?C . "☕")
     (?D . "🔬"))))


(provide 'tools/org)
;;; org.el ends here
