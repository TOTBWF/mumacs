
;;; lang/agda ---  -*- lexical-binding: t; -*-

;;; Commentary:
;; `agda2-mode' is a bit tricky to manage.  The version of Agda in use is
;; tied to a specific version of `agda2-mode'; this can cause problems when
;; working with multiple versions of Agda.  This gets particularly bad when
;; `nix' gets involved, as the `agda2-mode' files we need will be located
;; inside of the `nix' store.
;;
;; Luckily, `agda2-mode' comes with a solution for this in the form of
;; `agda2-set-program-version'.  This function unloads and reloads all of
;; the relevant `agda2-mode' files from Emacs with the appropriate version.
;; This even works with `nix', provided that the `agda-mode' binary is on
;; the PATH.
;;
;; However, this unload/reload process interacts rather poorly with `use-package',
;; as it will completely clobber any customization.  To avoid this problem, we place
;; all customization in an `with-eval-after-load' form inside of the `:init' block:
;; this ensures that any further reloads will cause the customization code to run again.


;;; Code:
(require 'core/tweaks)
(require 'core/path)

(require 'editor/snippets)

(create-file-template ".*.agda$" "agda-template" 'agda2-mode)
(create-file-template ".*.lagda.md$" "lagda-template" 'agda2-mode)

(use-package agda2-mode
  :preface
  (defun agda2-mode-display-fill-column ()
    "Enable `display-fill-column-indicator-mode' and set the fill column to 72."
    (display-fill-column-indicator-mode 1)
    (setq-local fill-column 72))
  :init
  ;; See COMMENTARY above.
  (with-eval-after-load 'agda2-mode
    (add-hook 'agda2-mode-hook #'agda2-mode-display-fill-column)
    (add-to-list 'auto-mode-alist '("\\.lagda\\.md\\'" . agda2-mode))
    (add-to-list 'compilation-error-regexp-alist-alist
		 '(agda "^[\s-]*\\(?:at \\)?\\(.*\\):\\([0-9]+\\),\\([0-9]+\\)-\\([0-9]+\\)$" 1 2 (3 . 4) 2 1))
    (add-to-list 'compilation-error-regexp-alist 'agda)))

(use-package agda-input
  :straight nil
  ;; We want to be able to call `set-input-method' with `Agda'
  ;; without opening an agda file, so we force `agda-input' to load
  ;; immediately.
  :demand t
  :functions agda-input-add-translations
  :init
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
       ("cc" . "𝒞")
       ("dd" . "𝒟")
       ("uu" . "⇑")
       ("wl" . "◀")
       ("wr" . "▶")
       ("cw" . "○")
       ("cf" . "●"))))
  :config
  ;; We have to explicitly `require' here to ensure that the input method is available.
  (require 'agda-input))



(provide 'lang/agda)
;;; agda.el ends here
