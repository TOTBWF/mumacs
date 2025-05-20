;;; lang/ocaml ---  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:
(require 'editor/company)

(use-package tuareg
  :ensure t
  :mode ("\\.ml'" "\\.mli'" "\\.opam'"))

(use-package merlin
  :ensure t
  :after tuareg
  :hook (tuareg-mode-hook . merlin-mode))

(use-package merlin-company
  :ensure t
  :after (merlin company)
  :company
  (tuareg-mode . merlin-company-backend))

(use-package merlin-eldoc
  :ensure t
  :after merlin
  :hook
  (tuareg-mode-hook . merlin-eldoc-setup))

(use-package merlin-iedit
  :ensure t
  :after merlin
  :bind
  (:map merlin-mode-map
	("C-;" . merlin-iedit-occurrences)))

(use-package utop
  :ensure t
  :hook
  (tuareg-mode-hhok . utop-minor-mode)
  :custom
  (utop-command "opam exec -- dune utop . -- -emacs"))

(use-package ocp-indent
  :ensure t
  :after tuareg
  :demand t
  :functions ocp-indent-region
  :config
  ;; ocp-indent-buffer is broken...
  (advice-add 'ocp-indent-buffer
              :override (lambda (&rest _r)
                          (ocp-indent-region (point-min) (point-max)))))

(use-package opam-switch-mode
  :ensure t
  :after tuareg
  :hook
  (tuareg-mode-hook . opam-switch-mode)
  :bind
  (:map opam-switch-mode-map
	("C-c C-w" . opam-switch-set-switch)))

(use-package dune
  :mode ("dune-project'" "dune'"))

(provide 'lang/ocaml)
;;; ocaml.el ends here
