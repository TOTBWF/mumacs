;;; lang/agda ---  -*- lexical-binding: t; -*-

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

(require 'core/tweaks)
(require 'core/path)
(require 'tools/direnv)

(require 'editor/spelling)
(require 'editor/snippets)

(use-package agda2-mode
  :ensure nil
  :load-path "~/.agda/share/2.8.0/emacs-mode/"
  :mode ("\\.lagda\\.md\\'" . agda2-mode)
  :spell-fu
  (agda2-mode-hook :include default)
  :custom
  ;; HACK: Agda broke itself again :)
  (agda2-program-name "/nix/store/l1nbh4rz28xfrvrpj3nnq44in6s4hxfp-ghc-9.4.6-with-packages/bin/agda"))

(use-package markdown-mode
  :ensure t
  ;; HACK: `markdown-mode' is a bad citizen, and clobbers `auto-mode-alist'
  ;; before it even loads. This means that anyone mentioning /anything/
  ;; `markdown-mode' related will trash the load order.
  ;; We actually need to force `markdown-mode' to load here; very frustrating!
  :demand t
  :config
  (add-to-list 'auto-mode-alist '("\\.lagda\\.md\\'" . agda2-mode)))

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
