;;; init --- The main init file -*- lexical-binding: t; no-byte-compile: t; -*-

;;; Commentary:

;;; Code:

;; Load the custom file first: this lets users perform
;; customizaton of things like `source-directory' early
;; on in the load process.
(load custom-file)
(add-to-list 'load-path (concat user-emacs-directory "modules"))

(setopt byte-compile-error-on-warn t)

;; Core editor functionality.
(require 'core/elpaca)
(require 'core/advice)
(require 'core/basics)
(require 'core/path)
(require 'core/tweaks)
(require 'core/meow)
(require 'core/help)
(require 'core/vertico)
(require 'core/backup)
(require 'core/transient)

(require 'editor/alert)
(require 'editor/auth-source)
(require 'editor/company)
(require 'editor/compilation)
(require 'editor/flymake)
(require 'editor/spelling)
;; (require 'editor/lsp)
(require 'editor/iedit)
(require 'editor/imenu)
(require 'editor/winner)
(require 'editor/project)
(require 'editor/snippets)

(require 'lang/agda)
(require 'lang/elisp)
(require 'lang/latex)
;; (require 'lang/lean)
(require 'lang/haskell)
(require 'lang/markdown)
(require 'lang/narya)
(require 'lang/nix)
(require 'lang/ocaml)
(require 'lang/prog)

(require 'tools/direnv)
(require 'tools/org)
(require 'tools/magit)
(require 'tools/notmuch)
(require 'tools/ripgrep)
(require 'tools/vterm)

(require 'theme/modus)
(require 'theme/tweaks)

(provide 'init)
;;; init.el ends here
