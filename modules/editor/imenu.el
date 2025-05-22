;;; editor/imenu ---  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(use-package imenu
  :ensure nil
  :bind
  (:map meow-leader-map
	("i" . imenu)))

(use-package imenu-list
  :commands imenu-list)

(provide 'editor/imenu)
;;; imenu.el ends here
