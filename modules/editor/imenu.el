;;; editor/imenu ---  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:
(require 'core/vertico)
(require 'core/meow)

(use-package imenu
  :ensure nil
  :bind
  (:map meow-leader-map
	("i" . consult-imenu)))

(use-package imenu-list
  :ensure t
  :commands imenu-list)

(provide 'editor/imenu)
;;; imenu.el ends here
