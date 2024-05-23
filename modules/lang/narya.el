;;; lang/narya ---  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:
(use-package narya-mode
  :straight
  (narya-mode :type git
	      :host github
	      :repo "mikeshulman/narya"
	      :files ("elisp/narya-mode.el")))

(provide 'lang/narya)
;;; narya.el ends here
