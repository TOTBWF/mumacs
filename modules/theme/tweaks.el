;;; theme/tweaks ---  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

;; Add a bit of padding around internal borders.
(add-to-list 'default-frame-alist '(internal-border-width . 48))

;; Pragmata Pro, 14 pt font as default font.
(set-face-attribute 'default nil :family "PragmataPro Mono" :height 140)

(use-package battery
  :straight nil
  :demand t
  :config
  (display-battery-mode 1))

(use-package time
  :straight nil
  :demand t
  :custom
  (display-time-default-load-average nil)
  :config
  (display-time-mode 1))

(use-package tab-bar
  :straight nil
  :demand t
  :custom
  (tab-bar-format '(tab-bar-format-history tab-bar-format-global))
  :config
  (tab-bar-mode 1))





(provide 'theme/tweaks)
;;; tweaks.el ends here
