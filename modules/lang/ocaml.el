;;; lang/ocaml ---  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:
(require 'editor/company)
(require 'core/path)

(use-package tuareg
  :ensure t
  :mode ("\\.ml'" "\\.mli'" "\\.opam'")
  :bind
  (:map tuareg-mode-map
        ("C-c C-o" . ff-find-other-file)))

;; (use-package merlin
;;   :ensure t
;;   :after tuareg
;;   :hook (tuareg-mode-hook . merlin-mode))

;; (use-package merlin-company
;;   :ensure t
;;   :after (merlin company)
;;   :company
;;   (tuareg-mode . merlin-company-backend))

;; (use-package merlin-eldoc
;;   :ensure t
;;   :after merlin
;;   :hook
;;   (tuareg-mode-hook . merlin-eldoc-setup))

;; (use-package merlin-iedit
;;   :ensure t
;;   :after merlin
;;   :bind
;;   (:map merlin-mode-map
;; 	("C-;" . merlin-iedit-occurrences)))

(use-package utop
  :ensure t
  :hook
  (tuareg-mode-hook . utop-minor-mode)
  :advice
  (utop :around inheritenv-apply)
  :custom
  (utop-command "dune utop . -- -emacs"))

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

;; (use-package opam-switch-mode
;;   :ensure t
;;   :after tuareg
;;   :hook
;;   (tuareg-mode-hook . opam-switch-mode)
;;   :bind
;;   (:map opam-switch-mode-map
;; 	("C-c C-w" . opam-switch-set-switch)))

(use-package dune
  :ensure t
  :mode ("dune-project'" "dune'"))

(provide 'lang/ocaml)
;;; ocaml.el ends here
