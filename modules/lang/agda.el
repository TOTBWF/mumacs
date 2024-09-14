;;; lang/agda ---  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:
(require 'core/tweaks)
(require 'core/path)

;; We need to load `agda2.el' from the nix store, as we need to use the version of Agda that
;; comes with the 1Lab.
(defconst agda-nix-path "/nix/store/qkcf5gjs2g5rzjc63jf4qhz5n6kyknjd-Agda-2.8.0-data/share/ghc-9.4.6/x86_64-osx-ghc-9.4.6/Agda-2.8.0/emacs-mode/agda2.el")
(add-to-path "/nix/store/k1zz4baymxqaqxfw9s2281wk4pksj999-ghc-9.4.6-with-packages/bin")

(load-file agda-nix-path)

(use-package agda2-mode
  :straight nil
  :mode "\\.lagda\\.md\\'"
  :custom
  (require 'meow)
  (agda2-program-name "/nix/store/k1zz4baymxqaqxfw9s2281wk4pksj999-ghc-9.4.6-with-packages/bin/agda")

  :hook
  (agda2-mode . display-fill-column-indicator-mode)
  :config
  (create-file-template ".*.agda$" "agda-template" 'agda2-mode)
  (create-file-template ".*.lagda.md$" "lagda-template" 'agda2-mode)
  (set-fill-column 72)

  (add-to-list 'compilation-error-regexp-alist-alist
	       '(agda "^[\s-]*\\(?:at \\)?\\(.*\\):\\([0-9]+\\),\\([0-9]+\\)-\\([0-9]+\\)$" 1 2 (3 . 4) 2 1))
  (add-to-list 'compilation-error-regexp-alist 'agda))

(use-package agda-input
  :straight nil
  :defer nil
  :config
  (require 'agda-input)
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

(provide 'lang/agda)
;;; agda.el ends here
