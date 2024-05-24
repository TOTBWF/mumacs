;;; init --- The main init file -*- lexical-binding: t; no-byte-compile: t; -*-

;;; Commentary:

;;; Code:
(add-to-list 'load-path "~/.config/emacs/modules")
(setq byte-compile-error-on-warn t)

;; Core editor functionality.
(require 'core/straight)
(require 'core/path)
(require 'core/tweaks)
(require 'core/meow)
(require 'core/help)
(require 'core/selectrum)
(require 'core/backup)

(require 'editor/company)
(require 'editor/compilation)
(require 'editor/flycheck)
(require 'editor/lsp)
(require 'editor/projectile)
(require 'editor/snippets)
(require 'editor/polymode)

(require 'lang/agda)
(require 'lang/elisp)
(require 'lang/latex)
(require 'lang/lean)
(require 'lang/narya)
(require 'lang/nix)
(require 'lang/ocaml)
(require 'lang/prog)

(require 'tools/magit)
(require 'tools/vterm)

(require 'theme/modus)
(require 'theme/tweaks)

;; Theme
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("b29ba9bfdb34d71ecf3322951425a73d825fb2c002434282d2e0e8c44fce8185"
     default))
 '(safe-local-variable-values
   '((projectile-project-compilation-cmd . "1lab-shake all -j"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(provide 'init)
;;; init.el ends here
