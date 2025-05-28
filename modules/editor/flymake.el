;;; editor/flymake --- Flymake configuration -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'core/meow)

(define-leader meow-flymake-leader-map "e" "error")

(use-package flymake
  :ensure nil
  :bind
  (:map meow-flymake-leader-map
	("e" . flymake-show-buffer-diagnostics)
	("j" . flymake-goto-next-error)
	("k" . flymake-goto-prev-error)))

(provide 'editor/flymake)
;;; flymake.el ends here
