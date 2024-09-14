;;; lang/rzk ---  -*- lexical-binding: t; -*-

;;; Commentary:
(require 'lsp-mode)

(add-to-path "~/.cabal/bin")

;;; Code:
(define-derived-mode rzk-mode prog-mode "Rzk"
  "Major mode for editing `rzk' files.
Shortcuts for interacting with `narya-mode':
\\{rzk-mode-map}"
  (setq-local comment-start "-- "))

(add-to-list 'lsp-language-id-configuration '(rzk-mode . "rzk"))
(lsp-register-client (make-lsp-client
                      :new-connection (lsp-stdio-connection '("rzk" "lsp"))
                      :activation-fn (lsp-activate-on "rzk")
                      :server-id 'rzk))


;;;###autoload
(add-to-list 'auto-mode-alist '("\\.rzk\\'" . rzk-mode))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.rzk.md\\'" . rzk-mode))

(provide 'lang/rzk)
;;; rzk.el ends here
