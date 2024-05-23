;;; lang/ocaml ---  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:
(require 'editor/company)

(defun ocaml-switch-interface ()
  "Switch between an interface file, and it's implementation."
  (interactive)
  (let* ((fname (buffer-file-name))
	 (ext (pcase (file-name-extension fname)
		("ml" "mli")
		("mli" "ml")
		(_ (error "Cannot find interface file for %s" fname)))))
    (find-file (concat (file-name-sans-extension fname) "." ext))))

(use-package tuareg
  :straight t
  :config
  (define-key tuareg-mode-map (kbd "C-c C-s") #'ocaml-switch-interface))

(use-package merlin
  :after tuareg
  :hook (tuareg-mode . merlin-mode))

(use-package merlin-company
  :after merlin
  :hook
  (tuareg-mode . company-mode)
  :company
  (tuareg-mode . merlin-company-backend))

(use-package merlin-eldoc
  :after merlin
  :hook
  (tuareg-mode . merlin-eldoc-setup))

(use-package merlin-iedit
  :after merlin
  :bind
  (:map merlin-mode-map
	("C-;" . merlin-iedit-occurances)))

(use-package ocp-indent
  :after tuareg
  :preface
  ;; Silence the byte compiler
  (declare-function ocp-indent-region "ocp-indent")
  :config
  ;; ocp-indent-buffer is broken...
  (advice-add 'ocp-indent-buffer
              :override (lambda (&rest _r)
                          (ocp-indent-region (point-min) (point-max)))))

(use-package opam-switch-mode
  :after tuareg
  :hook (tuareg-mode . opam-switch-mode)
  :bind
  (:map opam-switch-mode-map
	("C-c C-w" ("set switch" . opam-switch-set-switch))))

(provide 'lang/ocaml)
;;; ocaml.el ends here
