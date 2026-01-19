;;; editor/lsp ---  -*- lexical-binding: t; -*-

;;; Commentary:

;; Configuration for the various LSP clients that Emacs supports.

;;; Code:
(require 'editor/company)
(require 'editor/flycheck)

(require 'use-package-core)

(require 'cl-lib)

;;; Eglot:
(use-package eglot
  ;; Disable for now
  :ensure nil
  :after company
  :commands eglot
  :custom
  ;; Disable inlay hints, as they make a lot of requests to the
  ;; server while scrolling.
  (eglot-ignored-server-capabilities '(:inlayHintProvider))
  :hook
  (eglot-hook . company-mode))

;;;###autoload
(defun eglot-use-package--eglot-normalize (_name keyword args)
  "Normalizer for `:eglot' in `use-package' forms.
NAME, KEYWORD, and ARGS are explained in the `use-package' documentation."
  (mapcar
    (lambda (arg)
      ;; [FIXME: Reed M, 13/01/2026] Do some more robust validation following
      ;; `eglot-server-programs'.
      (pcase arg
        (`(,mode . ,contact) (cons mode contact))
        ((and (pred symbolp) mode) mode)
        (_ (use-package-error
            (concat (symbol-name keyword) "a <cons cell>")))))
    args))

(defun eglot-use-package--update-server-program (mode contact)
  "Update the CONTACT for a MODE in `eglot-server-programs'."
  (setcdr (assoc mode eglot-server-programs
                 (lambda (elt key)
                   (cond
                    ((symbolp elt) (eq key elt))
                    ((listp elt) (member key elt)))))
          contact))

;;;###autoload
(defun eglot-use-package--lsp-handler (name _keyword args rest state)
  "Handler for `:eglot' in `use-package' forms.
NAME, KEYWORD, ARGS, REST, and STATE are explained in
`use-package' documentation."
  (use-package-concat
   (use-package-process-keywords name rest state)
   (mapcan
    (lambda (arg)
      (pcase arg
        (`(,mode . ,contact)
         `((eval-after-load 'eglot
             (quote (eglot-use-package--update-server-program (quote ,mode) (eval ,contact))))
           (add-hook
            (quote ,(intern (concat (symbol-name mode) "-hook")))
            (function eglot-ensure))))
        ((and (pred symbolp) mode)
         `(add-hook
           (quote ,(intern (concat (symbol-name mode) "-hook")))
           (function eglot-ensure)))))
    (use-package-normalize-commands args))))

(defalias 'use-package-normalize/:eglot 'eglot-use-package--eglot-normalize)
(defalias 'use-package-handler/:eglot 'eglot-use-package--lsp-handler)

(add-to-list 'use-package-keywords ':eglot t)

(provide 'editor/lsp)
;;; lsp.el ends here
