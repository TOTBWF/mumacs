;;; editor/imenu ---  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(use-package imenu
  :straight nil
  :bind
  (:map meow-leader-keymap
	("i" ("imenu" . imenu))))

(use-package imenu-list)

(provide 'editor/imenu)
;;; imenu.el ends here
