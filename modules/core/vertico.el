;;; core/vertico.el ---  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:
(require 'core/straight)
(require 'core/meow)

(use-package vertico
  :straight (:files (:defaults "extensions/*"))
  :commands vertico-mode
  :demand t
  :init
  (vertico-mode))

(use-package vertico-prescient
  :after vertico
  :demand t
  :functions vertico-prescient-mode
  :config
  (vertico-prescient-mode 1))

;; `consult' provides a bunch of `completing-read' functions for
;; various search operations.
(use-package consult
  :demand t)

;; `ctrlf' is a handy way of searching within a buffer.
(use-package ctrlf
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
