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
  :preface
  (defun resize-org-latex-overlays ()
    "Resize all org latex previews in the current buffer."
    (cl-loop for o in (car (overlay-lists))
	     if (eq (overlay-get o 'org-overlay-type) 'org-latex-overlay)
	     do (plist-put (cdr (overlay-get o 'display))
			   :scale (expt text-scale-mode-step
					text-scale-mode-amount))))

  (defun org-mode-add-hooks ()
    "Hook for adding further minor-mode hooks when we are in `org-mode'."
    (add-hook 'text-scale-mode-hook #'resize-org-latex-overlays nil t))
  :hook
  (org-mode . org-cdlatex-mode)
  (org-mode . org-mode-add-hooks)
  :custom
  ;; Linking settings
  (org-id-link-to-org-use-id 'create-if-interactive)
  ;; TODO settings
  (org-todo-keywords
   '((sequence "TODO(t)" "OPEN(o)" "WAIT(w)" "|" "DONE(d)" "STOP(s)")))
  (org-todo-keyword-faces
   '(("OPEN" . font-lock-constant-face)
     ("WAIT" . font-lock-builtin-face)
     ("STOP" . font-lock-comment-face)))
  (org-log-done 'time)
  ;; Agenda settings
  (org-agenda-custom-commands
   '(("n" "Agenda and all tasks"
      ((agenda ""
	       ((org-agenda-skip-function '(org-agend-skip-entry-if-blocked-or-done))))
       (tags-todo "+category={task}+todo={TODO\\|OPEN}-blocked={t}"
		  ((org-agenda-sorting-strategy
		    '(priority-down todo-state-down))
		   (org-agenda-overriding-header
		    "Active tasks")))
       (tags-todo "+category={task}+todo={TODO\\|OPEN}+blocked={t}"
		  ((org-agenda-sorting-strategy
		    '(priority-down todo-state-down))
		   (org-agenda-overriding-header
		    "Blocked tasks")))
       (tags-todo "+category={task}+todo={WAIT}"
		  ((org-agenda-overriding-header
		    "Blockers")))))))
  (org-agenda-use-time-grid nil)
  ;; LaTeX settings
  ;; Add `mlmodern' as our font, and add the `microtype' package.
  ;; We modify this instead of `org-latex-packages-alist' to ensure
  ;; that we load our font early enough.
  (org-latex-default-packages-alist
   '(("" "amsmath" t ("lualatex" "xetex"))
     ("" "fontspec" t ("lualatex" "xetex"))
     ("" "mlmodern" t ("pdflatex"))
     ("AUTO" "inputenc" t ("pdflatex"))
     ("T1" "fontenc" t ("pdflatex"))
     ("" "microtype" t ("pdflatex"))
     ("" "graphicx" t)
     ("" "longtable" nil)
     ("" "wrapfig" nil)
     ("" "rotating" nil)
     ("normalem" "ulem" t)
     ("" "amsmath" t ("pdflatex"))
     ("" "amssymb" t ("pdflatex"))
     ("" "capt-of" nil)
     ("" "hyperref" nil)))
  ;; Add non-typesetting related packages.
  (org-latex-packages-alist
   '(("" "tikz" t)
     ("" "tikz-cd" t)
     ("" "quiver" t)))
  ;; Default to using `dvisvgm'.
  (org-preview-latex-default-process 'dvisvgm)
  ;; `dvisvgm' needs to have a `nil' background color, or it will
  ;; insert a rectangle that causes the entire SVG to become white.
  ;; Also take the time to bump up the scale.
  (org-format-latex-options
   '(:foreground "White" :background nil :scale 1.65 :html-foreground
		 "Black" :html-background "Black" :html-scale 1.0
		 :matchers ("begin" "$1" "$" "$$" "\\(" "\\[")))
  ;; Highlight LaTeX snippets in org buffers.
  (org-highlight-latex-and-related '(latex))
  ;; Org babel
  (org-babel-load-languages
   '((emacs-lisp . t)
     (haskell . t)))
  :config
  (defun org-agend-skip-entry-if-blocked-or-done ()
    "Skip all `org-agenda' entries that are either blocked or marked done."
    (and (or (org-entry-blocked-p)
	     (org-entry-is-done-p))
	 (org-entry-end-position))))


;; `org-timeblock' lets get a better daily view.
(use-package org-timeblock
  :commands org-timeblock org-timeblock-list
  :custom-face
  (org-timeblock-teaching-face ((t (:background "#00bcff"))))
  :custom
  (org-timeblock-tag-colors
   '(("teaching" . org-timeblock-teaching-face))))

(use-package ol-man
  :straight nil
  :after org)

;; Extensible dependencies for `org'.
(use-package org-edna
  :diminish org-edna-mode
  :hook (org-load . org-edna-mode))

;; Zotero link integration
(use-package zotxt
  :diminish org-zotxt-mode
  :hook (org-mode . org-zotxt-mode))

(use-package org-roam
  :after org
  :functions
  org-roam-db-autosync-mode
  org-roam-note-find
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
      (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n#+category: event\n* ${title}\n%^t")
      :unnarrowed t)
     ("p" "person" plain "%?" :target
      (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n#+category: person\n* ${title}")
      :unnarrowed t)))
  ;; HACK: This will get quite slow after a while; should use something smarter as detailed
  ;; in https://d12frosted.io/posts/2021-01-16-task-management-with-roam-vol5.html
  (org-agenda-files '("~/Documents/Notes/"))
  :config
  (defun org-roam-node-note-p (node)
    "Return `nil' if a node is a task, and `t' otherwise"
    (not (string-equal-ignore-case (org-roam-node-category node) "task")))

  (defun org-roam-note-find ()
    "Find and open an Org-roam node that is not a task."
    (interactive current-prefix-arg)
    (org-roam-node-find nil nil 'org-roam-node-note-p))
  :bind
  (:map meow-org-keymap
	("a" . org-agenda)
	("o" . org-roam-note-find)
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
     (?D . "🧊"))))


(provide 'tools/org)
;;; org.el ends here
