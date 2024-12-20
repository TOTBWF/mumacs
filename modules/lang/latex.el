;;; lang/latex ---  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:
(require 'editor/spelling)
(require 'core/path)
(require 'outline)
(require 'info)

(setq meow-latex-map (make-keymap))
(meow-define-state latex
  "Meow state for interacting with latex documents."
  :lighter "[L]"
  :keymap meow-latex-map)

(setq meow-cursor-type-latex 'hollow)

;; HACK: This is really bad: I should be adding this as a per-mode binding that resides in the local map.
(defun turn-on-meow-latex ()
  "Add bindings for `meow-latex-mode' into the `meow-normal-state-map'"
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

(use-package auctex
  :hook (LaTeX-mode . turn-on-meow-latex)
  :hook (LaTeX-mode . outline-minor-mode)
  :custom
  (font-latex-user-keyword-classes
   '(("citations" (("cite" "{") ("citep" "{") ("citet" "{")) font-lock-constant-face command)))
  (LaTeX-verbatim-environments
   '("code" "verbatim" "verbatim*" "filecontents" "filecontents*"))
  :bind
  (:map meow-latex-map
	("S" . LaTeX-section)))

;; Set up `spell-fu', and exclude some faces from spellchecking.
(use-package spell-fu
  :preface
  (require 'spell-fu)
  (defun spell-fu-LaTeX-hook ()
    (message "Spell fu!")
    (setq spell-fu-faces-include '(default))
    (spell-fu-mode 1))
  :hook (LaTeX-mode . spell-fu-LaTeX-hook))


(use-package reftex
  :straight nil
  :hook (LaTeX-mode . turn-on-reftex))

(use-package company-reftex
  :company
  (LaTeX-mode . company-reftex-citations))

(use-package company-auctex
  :hook
  (LaTeX-mode . company-mode)
  :company
  (LaTeX-mode . company-auctex-labels)
  (LaTeX-mode . (company-autex-macros company-auctex-symbols company-auctex-environments)))

(use-package cdlatex
  :preface
  :hook
  (LaTeX-mode . turn-on-cdlatex)
  :custom
  (cdlatex-sub-super-scripts-outside-math-mode nil)
  (cdlatex-takeover-dollar nil)
  :bind
  (:map meow-latex-map
	("e" . cdlatex-environment)))

(use-package math-delimiters
  :bind
  (:map cdlatex-mode-map
	("$" . nil))
  (:map LaTeX-mode-map
	("$" . math-delimiters-insert)))

(use-package tex-parens
  :straight (tex-parens :type git :host github :repo "ultronozm/tex-parens.el") ;; HACK: This is on GNU ELPA but straight.el can't find it?
  :hook
  (LaTeX-mode . tex-parens-mode)
  :bind
  (:map meow-latex-map
	("(" . tex-parens-backward-sexp)
	(")" . tex-parens-forward-sexp)
	("DEL" . tex-parens-delete-pair)))

;; This should probably live elsewhere.
(require 'f)
(require 'xwidget)

(defun xwidget-webkit-browse-tex ()
  (interactive)
  (xwidget-webkit-browse-url (f-swap-ext (format "file://%s" (buffer-file-name)) "pdf")))


(provide 'lang/latex)
;;; latex.el ends here
