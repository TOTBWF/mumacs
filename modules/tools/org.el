;;; tools/org ---  -*- lexical-binding: t; -*-

;;; Commentary:
;; We really want to use a version of `org-mode' provided via `elpaca',
;; so we make sure to strip out all occurances of the built-in `org' from the
;; load path.  This will prevent any annoying mistakes when calling `require'
;; on org-related files.  However, this needs to be done *extremely* early
;; in the load process; see `early-init.el' for the code that handles this.

;;; Code:
(require 'face-remap)

(require 'core/elpaca)
(require 'core/meow)
(require 'lang/latex)

;; Set up a `org' menu for `meow'.
(define-leader meow-org-leader-map "o" "org")

;; We ensure that `org' is handled via `elpaca' to make `org-roam' happy.
(use-package org
  :ensure t
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

  (defun org-xenops-dwim (&optional arg)
    "Enable `xenops-mode' if it is not already enabled,
and then invoke `xenops-dwim' with the prefix argument ARG."
    (interactive "P")
    (unless (and (featurep 'xenops) (xenops-mode))
      (xenops-mode 1))
    (xenops-dwim arg))

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
  (org-mode-hook . org-cdlatex-mode)
  (org-mode-hook . org-mode-add-hooks)
  :spell-fu
  (org-mode-hook :exclude org-meta-line org-link org-code)
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
     ("" "quiver" t)
     ("" "preamble" t))) ;; ~/Library/texmf/tex/latex/mystyles/
  ;; Default to using `dvisvgm'.
  (org-preview-latex-default-process 'dvisvgm)
  ;; `dvisvgm' needs to have a non-black background color, or it will
  ;; insert a rectangle that causes the entire SVG to become white.
  (org-format-latex-options
        '(:foreground "white" :background "#010101" :scale 1.65 :html-foreground
                      "white" :html-background "#010101" :html-scale 1.65
                      :matchers ("begin" "$1" "$" "$$" "\\(" "\\[")))
  ;; Similar hack is required for source blocks.
  (org-src-block-faces
        '(("latex" (:foreground "white" :background "#010101"))))
  ;; Also take the time to bump up the scale.
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
  (advice-add 'org-clock-in :after #'org-clock-in-mode-line-advice)
  :bind
  (:map org-mode-map
        ("$" . math-delimiters-insert)
        ("C-c C-x C-l" . org-xenops-dwim)))

(use-package org-agenda
  :ensure nil
  :demand t
  :after org
  :custom
  (org-agenda-inhibit-startup t)
  (org-agenda-use-time-grid nil)
  (org-agenda-tags-column -80)
  (org-agenda-prefix-format
   '((agenda . "  %?-12t% s")
     (todo . "  ")
     (tags . "  ")
     (search . "  ")))
  (org-agenda-custom-commands
   '(("n" "Agenda and all tasks"
      ((agenda
	""
	((org-agenda-skip-function '(org-agend-skip-entry-if-blocked-or-done))))
       (tags-todo
	"+todo={TODO\\|OPEN}-blocked={t}-borceux"
	((org-agenda-overriding-header "Active tasks")))
       (tags-todo
	"+task+todo={TODO\\|OPEN}+blocked={t}-borceux"
        ((org-agenda-overriding-header "Blocked tasks")))
       (tags-todo
	"+task+todo={WAIT}-borceux"
        ((org-agenda-overriding-header "Blockers"))))
      ((org-agenda-files (org-roam-ql-nodes-files '(or (tags "task") (tags "event") (tags "daily"))))
       (org-agenda-sorting-strategy
        '(priority-down todo-state-down))))
     ("b" "Borceux"
      ((tags-todo
	"+example+todo={TODO\\|OPEN}"
        ((org-agenda-overriding-header "Examples")
         (org-agenda-hide-tags-regexp "example")))
       (tags-todo
	"+definition+todo={TODO\\|OPEN}"
        ((org-agenda-overriding-header "Definitions")
         (org-agenda-hide-tags-regexp "definition")))
       (tags-todo
	"+proposition+todo={TODO\\|OPEN}"
        ((org-agenda-overriding-header "Propositions")
         (org-agenda-hide-tags-regexp "proposition")))
       (tags-todo
	"+exercise+todo={TODO\\|OPEN}"
        ((org-agenda-overriding-header "Exercises")
         (org-agenda-hide-tags-regexp "exercise"))))
      ((org-agenda-show-inherited-tags nil))
      (org-roam-ql-nodes-files '(tags "borceux")))))
  :config
  (defun org-agend-skip-entry-if-blocked-or-done ()
    "Skip all `org-agenda' entries that are either blocked or marked done."
    (and (or (org-entry-blocked-p)
             (org-entry-is-done-p))
         (org-entry-end-position))))

;; `org-timeblock' lets get a better daily view.
(use-package org-timeblock
  :ensure t
  :commands org-timeblock org-timeblock-list
  :custom-face
  (org-timeblock-teaching-face ((t (:background "#00bcff"))))
  :custom
  (org-timeblock-tag-colors
   '(("teaching" . org-timeblock-teaching-face))))

;; `ol-man' provides links to manpages.
(use-package ol-man
  :ensure nil
  :after org)

;; Extensible dependencies for `org'.
(use-package org-edna
  :ensure t
  :diminish org-edna-mode
  ;; `org-edna-mode' is a global mode, so we shouldn't attach it to `org-mode-hook'.
  ;; Moreover, `org-load-hook' is deprecated, so instead we opt to load it after org,
  ;; `:demand' it, and then immediately enable the mode.
  :after org
  :demand t
  :commands org-edna-mode
  :config
  (org-edna-mode)

  (defun org-edna-finder/nodes (&rest ids)
    "Find a list of org-roam nodes with given IDS.

Edna Syntax: roam-ids(ID1 ID2 ...)

Each ID is a UUID as understood by `org-roam-node-from-id'.

Note that in the edna syntax, the IDs don't need to be quoted."
    (mapcar
     (lambda (id)
       (let* ((node (org-roam-node-from-id id))
		  (file (org-roam-node-file node))
		  (buffer (find-file-noselect file)))
	     (save-window-excursion
	       (set-buffer buffer)
	       (org-next-visible-heading 1)
	       (point-marker))))
     ids)))

;; Zotero link integration
(use-package zotxt
  :ensure t
  :diminish
  org-zotxt-mode
  :hook
  (org-mode-hook . org-zotxt-mode))

(use-package org-roam
  :ensure t
  :after org
  :demand t
  :functions
  org-roam-db-autosync-mode
  org-roam-node-find
  org-roam-db-query
  ;; We need these for `org-edna-finder/nodes'.
  org-roam-node-marker
  org-roam-node-from-id
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

  (defun org-roam-get-template (file)
    "Construct the path to the template FILE."
    (f-join user-emacs-directory "templates" file))
  :custom
  ;; Make sure that `org-roam' uses the same directory as `logseq'.
  (org-roam-directory "~/Documents/Notes")
  (org-directory "~/Documents/Notes")
  (org-roam-dailies-directory "journals/")
  (org-roam-file-exclude-regexp '("data/" "logseq/bak/"))
  (org-roam-dailies-capture-templates
   `(("d" "default" plain
      (file ,(org-roam-get-template "daily.org"))
      :target (file "%<%Y-%m-%d>.org")
      :unnarrowed t)))
  (org-roam-capture-templates
   `(("n" "note" plain
      (file ,(org-roam-get-template "note.org"))
      :target (file "%<%Y%m%d%H%M%S>-${slug}.org")
      :unnarrowed t)
     ("t" "task" plain
      (file ,(org-roam-get-template "task.org"))
      :target (file "tasks/%<%Y%m%d%H%M%S>-${slug}.org")
      :unnarrowed t)
     ("l" "1lab task" plain
      (file ,(org-roam-get-template "1lab-task.org"))
      :target (file "tasks/%<%Y%m%d%H%M%S>-${slug}.org")
      :unnarrowed t)
     ("e" "event" plain
      (file ,(org-roam-get-template "event.org"))
      :target (file "events/%<%Y%m%d%H%M%S>-${slug}.org")
      :unnarrowed t)
     ("d" "default" plain
      (file ,(org-roam-get-template "daily.org"))
      :target (file "%<%Y-%m-%d>.org")
      :unnarrowed t)
     ("p" "person" plain
      (file ,(org-roam-get-template "person.org"))
      :target (file "%<%Y%m%d%H%M%S>-${slug}.org")
      :unnarrowed t)))
  :config
  ;; Keep the `org-roam' session synchronized.
  (org-roam-db-autosync-mode 1)
  :bind
  (:map org-mode-map
        ("C-c C-q" . org-roam-tag-add))
  (:map meow-org-leader-map
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
  :ensure t
  :after org
  :diminish org-fancy-priorities-mode
  :hook
  (org-mode-hook . org-fancy-priorities-mode)
  :custom
  (org-highest-priority ?A)
  (org-default-priority ?B)
  (org-lowest-priority ?D)
  (org-fancy-priorities-list
   '((?A . "❗")
     (?B . "⚠️")
     (?C . "☕")
     (?D . "🧊"))))

;; (use-package org-ql
;;   :after org
;;   :custom
;;   (org-ql-views
;;    '(("Notes: Empty Notes"
;;       :buffers-files org-roam-list-files
;;       :query
;;       (and (empty-entry) (tags "note") (level 1))
;;       :title "Empty Notes")
;;      ("Notes: Untagged Notes"
;;       :buffers-files org-roam-list-files
;;       :query
;;       (and (untagged "note") (level 1))
;;       :title "Untagged Notes")))
;;   :config
;;   (org-ql-defpred empty-entry ()
;;     "Return non-nil if the current entry contains no text beyond the headline."
;;     :body
;;     (save-excursion
;;       (save-match-data
;;         (forward-line)
;;         (not (re-search-forward "[^[:space:]]" (save-excursion (org-entry-end-position)) t 1)))))

;;   (org-ql-defpred untagged (&rest tags)
;;     "Return non-nil if the tags of the current entry are a subset of a list of tags."
;;     :body
;;     (cl-subsetp (org-get-tags) tags)))

(use-package org-roam-ql
  :ensure t
  :after org org-roam
  :autoload
  org-roam-ql-agenda-block
  org-roam-ql-nodes-files
  org-roam-ql-defpred)

;; TOO SLOW
;; (use-package org-upcoming-modeline
;;   :after org
;;   ;; FIXME: There are byte-compiler errors with org-ql.
;;   :no-require t
;;   :demand t
;;   :commands org-upcoming-modeline-mode
;;   :config
;;   (org-upcoming-modeline-mode 1))

(use-package org-roam-ui
  :ensure t
  :commands org-roam-ui)

(provide 'tools/org)
;;; org.el ends here
