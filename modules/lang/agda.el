;;; lang/agda ---  -*- lexical-binding: t; -*-

;;; Commentary:


;;; Code:
(require 'core/tweaks)
(require 'core/path)
(require 'tools/direnv)

(require 'editor/snippets)

(use-package agda2-mode
  :straight nil
  :load-path "~/.agda/share/2.8.0/emacs-mode/"
  :mode ("\\.lagda\\.md\\'" . agda2-mode)
  :spell-fu
  (agda2-mode-hook :include default)
  :custom
  ;; HACK: Agda broke itself again :)
  (agda2-program-name "/nix/store/l1nbh4rz28xfrvrpj3nnq44in6s4hxfp-ghc-9.4.6-with-packages/bin/agda"))

(use-package markdown-mode
  ;; HACK: `markdown-mode' is a bad citizen, and clobbers `auto-mode-alist'
  ;; before it even loads. This means that anyone mentioning /anything/
  ;; `markdown-mode' related will trash the load order.
  ;; We actually need to force `markdown-mode' to load here; very frustrating!
  :demand t
  :config
  (add-to-list 'auto-mode-alist '("\\.lagda\\.md\\'" . agda2-mode)))

(use-package compilation
  :straight nil
  :config
  (add-to-list 'compilation-error-regexp-alist-alist
	       '(agda "^[\s-]*\\(?:at \\)?\\(.*\\):\\([0-9]+\\),\\([0-9]+\\)-\\([0-9]+\\)$" 1 2 (3 . 4) 2 1))
  (add-to-list 'compilation-error-regexp-alist 'agda))

(use-package agda-input
  ;; We've already done the autoload pass of `agda2-mode', so we don't
  ;; need to fetch `agda-input' from upstream; we already know how to
  ;; `require' it.
  :straight nil
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
