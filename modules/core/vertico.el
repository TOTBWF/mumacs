;;; core/vertico.el ---  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:
(require 'core/elpaca)
(require 'core/meow)

(use-package vertico
  :ensure t
  :commands vertico-mode
  :hook
  (elpaca-after-init-hook . vertico-mode)
  :bind
  (:map vertico-map
	("C-<backspace>" . vertico-directory-up)))

(use-package vertico-prescient
  :ensure t
  :hook
  (vertico-mode-hook . vertico-prescient-mode))

;; `consult' provides a bunch of `completing-read' functions for
;; various search operations.
(use-package consult
  :ensure t)

;; `ctrlf' is a handy way of searching within a buffer.
(use-package ctrlf
  :ensure t
  :commands ctrlf-forward-literal ctrlf-backward-literal
  :functions ctrlf-change-search-style
  :preface
  (meow-define-keys
      'normal
    '("/" . ctrlf-forward-literal))
  (define-key Info-mode-map (kbd "/") #'ctrlf-forward-literal)
  :config
  (ctrlf-change-search-style 'fuzzy)
  (define-key ctrlf-mode-map (kbd "C-j") #'ctrlf-forward-literal)
  (define-key ctrlf-mode-map (kbd "C-k") #'ctrlf-backward-literal))


(provide 'core/vertico)
;;; vertico.el ends here
