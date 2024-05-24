;;; lang/agda ---  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:
(require 'core/tweaks)

;; We need to load `agda2.el' from the nix store, as we need to use the version of Agda that
;; comes with the 1Lab.
(defconst agda-nix-path "/nix/store/qmap97y4dww410jd5k6lglxf5apwlk96-Agda-2.6.5-data/share/ghc-9.4.6/x86_64-osx-ghc-9.4.6/Agda-2.6.5/emacs-mode/agda2.el")
(setq agda2-program-name "/nix/store/8xf6qyj435qw52s1n9nnakd0js7008xw-ghc-9.4.6-with-packages/bin/agda")
(add-to-path "/nix/store/8xf6qyj435qw52s1n9nnakd0js7008xw-ghc-9.4.6-with-packages/bin/")


(load-file agda-nix-path)
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
   ("cf" . "●")))

(provide 'lang/agda)
;;; agda.el ends here
