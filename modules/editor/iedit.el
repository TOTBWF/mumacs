;;; editor/iedit ---  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:


(use-package iedit
  :ensure t
  :bind
  (:map global-map
	("C-;" . iedit-mode)))

(provide 'editor/iedit)
;;; iedit.el ends here
