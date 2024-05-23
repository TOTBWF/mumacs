;;; theme/tweaks ---  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:
(require 'time)

;; Add a bit of padding around internal borders.
(add-to-list 'default-frame-alist '(internal-border-width . 48))

;; Pragmata Pro, 14 pt font as default font.
(set-face-attribute 'default nil :family "PragmataPro Mono" :height 140)

;; Display battery in the modeline.
(display-battery-mode 1)

;; Display time on the modeline, but get rid
(setq display-time-default-load-average nil)
(display-time-mode 1)

(provide 'theme/tweaks)
;;; tweaks.el ends here
