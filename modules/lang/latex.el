;;; lang/latex ---  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:
(require 'editor/spelling)
(require 'core/path)
(require 'outline)
(require 'info)

(defconst meow-latex-map (make-keymap))

(meow-define-state latex
  "Meow state for interacting with latex documents."
  :lighter "[L]"
  :keymap meow-latex-map)

(setq meow-cursor-type-latex 'hollow)

;; HACK: This is really bad: I should be adding this as a per-mode binding that resides in the local map.
(defun turn-on-meow-latex ()
  "Add bindings for `meow-latex-mode' into the `meow-normal-state-map'."
  (meow-define-keys 'normal '("C" . meow-latex-mode)))

(meow-define-keys 'latex
  '("<escape>" . meow-normal-mode)
  '("C" . meow-normal-mode)
  '("i" . meow-insert-mode)
  '("u" . meow-undo)
  '("SPC" . meow-keypad)
  ;; Outline mode settings
  '("<" . outline-promote)
  '(">" . outline-demote)
  '("p" . outline-previous-visible-heading)
  '("n" . outline-next-visible-heading)
  '("w" . outline-move-subtree-up)
  '("s" . outline-move-subtree-down))

(use-package latex
  :straight auctex
  :hook
  (LaTeX-mode-hook . turn-on-meow-latex)
  (LaTeX-mode-hook . outline-minor-mode)
  :spell-fu
  (LaTeX-mode-hook
   :exclude
   font-lock-constant-face
   font-lock-function-name-face
   font-latex-sedate-face
   font-latex-math-face)
  :custom
  (font-latex-user-keyword-classes
   '(("citations" (("cite" "{") ("citep" "{") ("citet" "{")) font-lock-constant-face command)))
  (LaTeX-verbatim-environments
   '("code" "verbatim" "verbatim*" "filecontents" "filecontents*"))
  :bind
  (:map LaTeX-mode-map
	("$" . math-delimiters-insert)
	)
  (:map meow-latex-map
	("S" . LaTeX-section)))

(use-package tex-mode
  :straight nil
  :bind
  (:map latex-mode-map
	("$" . math-delimiters-insert)))


(use-package reftex
  :straight nil
  :hook (LaTeX-mode-hook . turn-on-reftex))

(use-package company-reftex
  :company
  (LaTeX-mode-hook . company-reftex-citations))

(use-package company-auctex
  :company
  (LaTeX-mode-hook . (company-auctex-labels company-autex-macros company-auctex-symbols company-auctex-environments)))

(use-package cdlatex
  :hook
  (LaTeX-mode-hook . turn-on-cdlatex)
  (latex-mode-hook . turn-on-cdlatex)
  :custom
  (cdlatex-sub-super-scripts-outside-math-mode nil)
  (cdlatex-takeover-dollar nil)
  (cdlatex-env-alist
   '(("figure"
      "\\begin{figure}[!ht]\n\\caption[]{}\n\\end{figure}"
      )))
  :bind
  ;; Relinquish control of the `$' key.
  (:map cdlatex-mode-map
	("$" . nil))
  (:map meow-latex-map
	("e" . cdlatex-environment)))

(use-package math-delimiters
  :commands math-delimiters-insert)

(use-package xenops
  :preface
  (defun xenops--dont-use-drag-and-drop ()
    "Don't enable `mouse-drag-and-drop-region' in `xenops-mode'."
    (setq mouse-drag-and-drop-region nil))
  :hook
  (LaTeX-mode-hook . xenops-mode)
  :commands
  xenops-mode
  xenops-dwim
  :advice
  (xenops-math-activate :after xenops--dont-use-drag-and-drop)
  :config
  ;; HACK: `xenops-mode' defines `xenops-math-image-scale-factor' via `defvar'
  ;; instead of `defcustom', which makes `:custom' apply at the incorrect time.
  ;; To work around this, we need to apply this customization after the package loads.
  (setq xenops-math-image-scale-factor 1.65)
  :bind
  (:map LaTeX-mode-map
	("C-c C-x C-l" . xenops-dwim)))

(use-package tex-parens
  :straight (tex-parens :type git :host github :repo "ultronozm/tex-parens.el") ;; HACK: This is on GNU ELPA but straight.el can't find it?
  :hook
  (LaTeX-mode-hook . tex-parens-mode)
  (latex-mode-hook . tex-parens-mode)
  :bind
  (:map meow-latex-map
	("(" . tex-parens-backward-sexp)
	(")" . tex-parens-forward-sexp)
	("DEL" . tex-parens-delete-pair)))

;; This should probably live elsewhere.
(require 'f)
(require 'xwidget)

(defun xwidget-webkit-browse-tex ()
  "Open the PDF associated to TeX file in the current buffer in a webkit xwidget."
  (interactive)
  (xwidget-webkit-browse-url (f-swap-ext (format "file://%s" (buffer-file-name)) "pdf")))


(provide 'lang/latex)
;;; latex.el ends here
