;;; lang/tlaplus ---  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

;; (use-package tla-ts-mode
;;   :mode "\\.tla\\'"
;;   :straight
;;   (tla-ts-mode
;;    :type git
;;    :host github
;;    :repo "Davidbrcz/tla-ts-mode"
;;    :files ("tla-ts-mode.el"))
;;   :ensure t
;;   :config
;;   ; The grammar is called tlaplus, but the mode is called tla
;;   (setq treesit-load-name-override-list '((tla "libtree-sitter-tlaplus" "tree_sitter_tlaplus"))))

(use-package tla-mode
  :mode "\\.tla\\'"
    :straight
  (tla-mode
   :type git
   :host github
   :repo "ratish-punnoose/tla-mode"
   :files ("tla-mode.el"))
  )

(provide 'lang/tlaplus)
;;; tlaplus.el ends here
