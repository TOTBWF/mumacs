;;; core/custom ---  -*- lexical-binding: t; -*-

;;; Commentary:
;; Customizing the customization system.

;;; Code:

;; Customizations get stored in `custom.el'.
(setq custom-file (file-name-concat user-emacs-directory "custom.el"))
(load custom-file)

(provide 'core/custom)
;;; custom.el ends here
