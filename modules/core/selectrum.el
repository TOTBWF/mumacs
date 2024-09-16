;;; core/selectrum ---  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:
(require 'core/straight)
(require 'core/meow)

(require 'use-package)
(use-package selectrum
  :demand t
  :functions selectrum-mode
  :config
  (selectrum-mode 1))

(use-package selectrum-prescient
  :after selectrum
  :demand t
  :functions selectrum-prescient-mode
  :config
  (selectrum-prescient-mode 1))

;; `consult' provides a bunch of `completing-read' functions for
;; various search perations.
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

(provide 'core/selectrum)
;;; core/selectrum.el ends here
