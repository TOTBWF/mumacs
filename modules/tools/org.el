;;; tools/org ---  -*- lexical-binding: t; -*-

;;; Commentary:
;; We really want to use a version of `org-mode' provided via `straight',
;; so we make sure to strip out all occurances of the built-in `org' from the
;; load path.  This will prevent any annoying mistakes when calling `require'
;; on org-related files.  However, this needs to be done *extremely* early
;; in the load process; see `early-init.el' for the code that handles this.

;;; Code:
(require 'face-remap)

(require 'core/meow)

;; Set up a `org' menu for `meow'.
(defconst meow-org-keymap (define-keymap))
(meow-define-keys 'leader `("o" . ("org" . ,meow-org-keymap)))

;; We ensure that `org' is handled via `straight' to make `org-roam' happy.
(use-package org
  ;; HACK: we demand org to get keybindings to work.
  :demand t
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

  ;; We want to byte compile our advice, so we predeclare the functions
  ;; to make the byte compiler aware of them.
  (declare-function org-clock-out-mode-line-advice "tools/org.el")
  (declare-function org-clock-in-mode-line-advice "tools/org.el")
  :functions
  org-entry-blocked-p
  org-entry-is-done-p
  org-clocking-p
  :diminish
  org-cdlatex-mode
  :hook
  (org-mode . org-cdlatex-mode)
  (org-mode . org-mode-add-hooks)
  :custom-face
  (org-block ((t (:background nil))))
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
     ("" "stmaryrd" t ("pdflatex"))
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
  (org-highlight-latex-and-related '(native))
  ;; Org babel
  (org-babel-load-languages
   '((emacs-lisp . t)
     (shell . t)
     (haskell . t)))
  :config
  (defconst org-no-clock-mode-line-string
    (propertize "[No Clock] " 'face 'org-mode-line-no-clock))

  (defface org-mode-line-no-clock
    '((t (:inherit modus-themes-prominent-error)))
    "Face used to display that there are no clocked tasks in the mode line."
    :group 'org-faces)

  (defun org-clock-out-mode-line-advice (&rest _)
    (when (not (org-clocking-p))
      (setq global-mode-string (append global-mode-string '(org-no-clock-mode-line-string)))))

  (defun org-clock-in-mode-line-advice (&rest _)
    (delq 'org-no-clock-mode-line-string global-mode-string))

  (advice-add 'org-clock-out :after #'org-clock-out-mode-line-advice)
  (advice-add 'org-clock-in :after #'org-clock-in-mode-line-advice))

(use-package org-agenda
  :straight nil
  :demand t
  :after org
  :custom
  (org-agenda-use-time-grid nil)
  (org-agenda-custom-commands
   '(("n" "Agenda and all tasks"
      ((agenda ""
               ((org-agenda-skip-function '(org-agend-skip-entry-if-blocked-or-done))))
       (tags-todo "+task+todo={TODO\\|OPEN}-blocked={t}-borceux"
                  ((org-agenda-sorting-strategy
                    '(priority-down todo-state-down))
                   (org-agenda-overriding-header
                    "Active tasks")))
       (tags-todo "+task+todo={TODO\\|OPEN}+blocked={t}-borceux"
                  ((org-agenda-sorting-strategy
                    '(priority-down todo-state-down))
                   (org-agenda-overriding-header
                    "Blocked tasks")))
       (tags-todo "+task+todo={WAIT}-borceux"
                  ((org-agenda-overriding-header
                    "Blockers")))))
     ("b" "Borceux"
      ((tags-todo "+borceux+example+todo={TODO\\|OPEN}"
                  ((org-agenda-overriding-header "Examples")
                   (org-agenda-hide-tags-regexp "example")
                   (org-agenda-show-inherited-tags nil)))
       (tags-todo "+borceux+definition+todo={TODO\\|OPEN}"
                  ((org-agenda-overriding-header "Definitions")
                   (org-agenda-hide-tags-regexp "definition")
                   (org-agenda-show-inherited-tags nil)))
       (tags-todo "+borceux+proposition+todo={TODO\\|OPEN}"
                  ((org-agenda-overriding-header "Propositions")
                   (org-agenda-hide-tags-regexp "proposition")
                   (org-agenda-show-inherited-tags nil)))
       (tags-todo "+borceux+exercise+todo={TODO\\|OPEN}"
                  ((org-agenda-overriding-header "Exercises")
                   (org-agenda-hide-tags-regexp "exercise")
                   (org-agenda-show-inherited-tags nil)))))))
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
  ;; `org-edna-mode' is a global mode, so we shouldn't attach it to `org-mode-hook'.
  ;; Moreover, `org-load-hook' is deprecated, so instead we opt to load it after org,
  ;; `:demand' it, and then immediately enable the mode.
  :after org
  :demand t
  :commands org-edna-mode
  :config
  (org-edna-mode))

;; Zotero link integration
(use-package zotxt
  :diminish
  org-zotxt-mode
  :hook (org-mode . org-zotxt-mode))

(use-package org-roam
  :after org
  :demand t
  :functions
  org-roam-db-autosync-mode
  org-roam-node-find
  org-roam-db-query
  :preface
  (defun org-roam-node-note-p (node)
    "Return `nil' if a node is a node, and `t' otherwise"
    (not (member "task" (org-roam-node-tags node))))

  (defun org-roam-node-task-p (node)
    "Return `nil' if a node is a task, and `t' otherwise"
    (member "task" (org-roam-node-tags node)))

  (defun org-roam-note-find ()
    "Find and open an Org-roam node that is not a task."
    (interactive current-prefix-arg)
    (org-roam-node-find nil nil 'org-roam-node-note-p))

  (defun org-roam-task-find ()
    "Find and open an Org-roam node that is a task."
    (interactive current-prefix-arg)
    (org-roam-node-find nil nil 'org-roam-node-task-p))
  :config
  ;; Keep the `org-roam' session synchronized.
  (org-roam-db-autosync-mode 1)
  :custom
  ;; Make sure that `org-roam' uses the same directory as `logseq'.
  (org-roam-directory "~/Documents/Notes")
  (org-roam-dailies-directory "journals/")
  (org-roam-capture-templates
   '(("n" "note" plain "%?" :target
      (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n#+filetags: :note:\n* ${title}\n")
      :unnarrowed t)
     ("t" "task" plain "%?" :target
      (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n#+filetags: :task:\n* TODO [#B] ${title}")
      :unnarrowed t)
     ("l" "1lab task" plain "%?" :target
      (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n#+filetags: :task:1lab:\n* TODO [#B] ${title}")
      :unnarrowed t)
     ("e" "event" plain "%?" :target
      (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n#+filetags: :event:\n* ${title}\n%^t")
      :unnarrowed t)
     ("p" "person" plain "%?" :target
      (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n#+filetags: :person:\n* ${title}")
      :unnarrowed t)))
  (org-agenda-files '("~/Documents/Notes/"))
  :bind
  (:map meow-org-keymap
        ("a" . org-agenda)
        ("o" . org-roam-note-find)
        ("t" . org-roam-task-find)
        ("c" . org-roam-capture)
        ("l" . org-store-link)
        ("x j" . org-clock-goto)
        ("x o" . org-clock-out)
        ("x i" . org-clock-in-last)
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

;; TOO SLOW
;; (use-package org-upcoming-modeline
;;   :after org
;;   ;; FIXME: There are byte-compiler errors with org-ql.
;;   :no-require t
;;   :demand t
;;   :commands org-upcoming-modeline-mode
;;   :config
;;   (org-upcoming-modeline-mode 1))

(provide 'tools/org)
;;; org.el ends here
