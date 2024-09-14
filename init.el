;;; init --- The main init file -*- lexical-binding: t; no-byte-compile: t; -*-

;;; Commentary:

;;; Code:
(add-to-list 'load-path "~/.config/emacs/modules")
(setq byte-compile-error-on-warn t)

;; Core editor functionality.
(require 'core/straight)
(require 'core/path)
(require 'core/custom)
(require 'core/tweaks)
(require 'core/meow)
(require 'core/help)
(require 'core/selectrum)
(require 'core/backup)

(require 'editor/company)
(require 'editor/compilation)
(require 'editor/flycheck)
(require 'editor/spelling)
(require 'editor/lsp)
(require 'editor/imenu)
(require 'editor/projectile)
(require 'editor/snippets)
;; (require 'editor/polymode)

(require 'lang/agda)
(require 'lang/elisp)
(require 'lang/latex)
(require 'lang/lean)
(require 'lang/haskell)
;; (require 'lang/narya)
(require 'lang/nix)
(require 'lang/ocaml)
(require 'lang/prog)

(require 'tools/magit)
(require 'tools/direnv)
(require 'tools/org)
(require 'tools/vterm)

(require 'theme/modus)
(require 'theme/tweaks)

(provide 'init)
;;; init.el ends here
