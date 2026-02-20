;;; lang/agda ---  -*- lexical-binding: t; byte-compile-warnings: (not free-vars) -*-

;;; Commentary:
;; `agda2-mode' is a bit tricky to manage.  The version of Agda in use is
;; tied to a specific version of `agda2-mode'; this can cause problems when
;; working with multiple versions of Agda.  This gets particularly bad when
;; `nix' gets involved, as the `agda2-mode' files we need will be located
;; inside of the `nix' store.
;;
;; This leads us to the following hack of a configuration, which hard-codes
;; paths left and right.  This works for now, but is not sustainable in the
;; long term.  I've attempted some other solutions (see 90b32ce) but nothing
;; has quite worked out.

;;; Code:
(require 'compile)

(require 'core/elpaca)

(require 'core/path)
(require 'tools/direnv)

(require 'editor/lsp)
(require 'editor/spelling)
(require 'editor/snippets)



(use-package agda2-mode
  :ensure
  (agda2-mode
   :type git
   :host github
   :repo "agda/agda"
   :files ("src/data/emacs-mode/*.el"))
  ;; :load-path "/Users/reedmullanix/.cabal/share/x86_64-osx-ghc-9.12.2-5a96/Agda-2.9.0/emacs-mode/"
  :commands agda2-mode
  :preface
  ;; See [NOTE: markdown-mode and auto-mode-alist]
  (defun agda-mode@repair-auto-mode-alist ()
    "Repair the damage done to `auto-mode-alist' by `markdown-mode'."
    (add-to-list 'auto-mode-alist '("\\.lagda\\.md\\'" . agda2-mode)))

  (defun agda2-mode@envrc-update ()
    (envrc--update)
    (let ((agda-bin (executable-find "agda")))
      (unless (equal agda-bin agda2-program-name)
        (let* ((agda-buffers
                (cl-mapcan (lambda (buf)
                             (with-current-buffer buf
                               (when (equal major-mode 'agda2-mode)
                                 (list buf))))
                           (buffer-list)))
               (default-hook (default-value 'agda2-mode-hook))
               (agda-mode-path
                (with-temp-buffer
                  (call-process agda-bin nil (current-buffer) nil "--emacs-mode" "locate")
                  (buffer-string))))

          ;; Remove the Agda mode directory from the load path.
          (setq load-path (delete agda2-directory load-path))

          ;; Unload the Agda mode and its dependencies.
          (unload-feature 'agda2-mode      'force)
          (unload-feature 'agda2           'force)
          (unload-feature 'eri             'force)
          (unload-feature 'annotation      'force)
          (unload-feature 'agda-input      'force)
          (unload-feature 'agda2-highlight 'force)
          (unload-feature 'agda2-abbrevs   'force)
          (unload-feature 'agda2-queue     'force)

          ;; Load the new version of Agda.
          (load-file agda-mode-path)
          (require 'agda2-mode)

          (when default-hook
            (set-default 'agda2-mode-hook default-hook))
          (setq agda2-program-name agda-bin)
          (mapc (lambda (buf)
                  (with-current-buffer buf
                    (agda2-mode)))
                agda-buffers)))))
  :hook
  (elpaca-after-init-hook . agda-mode@repair-auto-mode-alist)
  :spell-fu
  (agda2-mode-hook :include default)
  ;; HACK: we run `envrc--update' before restarting `agda' to make sure
  ;; that we can find versions of Agda that might not be on our global path.
  ;; This is still a bit of a stop-gap fix, as we can't easily have multiple
  ;; versions of `agda2-mode' running side-by-side, but its better than nothing.
  :advice
  (agda2-mode :before agda2-mode@envrc-update)
  :bind
  (:map agda2-mode-map
	("C-c C-x C-;" . agda2-comment-dwim-rest-of-buffer)))


(use-package compilation
  :ensure nil
  :config
  (add-to-list 'compilation-error-regexp-alist-alist
	       '(agda "^[\s-]*\\(?:at \\)?\\(.*\\):\\([0-9]+\\),\\([0-9]+\\)-\\([0-9]+\\)$" 1 2 (3 . 4) 2 1))
  (add-to-list 'compilation-error-regexp-alist 'agda))

(use-package agda-input
  :ensure
  (agda-input
   :host github
   :repo "agda/agda"
   :files (:defaults "src/data/emacs-mode/agda-input.el"))
  ;; We want to be able to call `set-input-method' with `Agda'
  ;; without opening an agda file, so we force `agda-input' to load
  ;; immediately.
  :demand t
  :functions agda-input-add-translations
  :preface
  ;; See COMMENTARY above.
  (with-eval-after-load 'agda-input
    (agda-input-add-translations
     '(("hom" . "⇒")
       ("lam" . "λ")
       ("lam-" . "ƛ")
       ("iso" . "≅")
       ("embed" . "↪")
       ("mono" . "↣")
       ("epi" . "↠")
       ("tail" . "⤚")
       ("nat" . "ℕ")
       ("int" . "ℤ")
       ("alpha" . "α")
       ("beta" . "β")
       ("gamma" . "γ")
       ("yo" . "よ")
       ("inv" . "⁻¹")
       ("monus" . "∸")
       ("U" . "⋃")
       ("lsq" . "⊑")
       ("ls" . "⊏")
       ("cc" . "𝒞")
       ("dd" . "𝒟")
       ("uu" . "⇑")
       ("wl" . "◀")
       ("wr" . "▶")
       ("cw" . "○")
       ("cf" . "●")
       ("mm" . "↦")
       ("fl" . "⧵")
       ("fr" . "/")
       ("bar" . "❚")
       )))
  :config
  ;; We have to explicitly `require' here to ensure that the input method is available.
  (require 'agda-input))

(provide 'lang/agda)
;;; agda.el ends here
