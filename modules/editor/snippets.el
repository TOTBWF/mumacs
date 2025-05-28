;;; editor/snippets ---  -*- lexical-binding: t; -*-

;;; Commentary:
;; I use `yasnippet' for all my snippeting needs, and I use it pretty heavily!
;;
;; I also use a combination of `autoinsert' and `yasnippet' to insert file headers
;; automatically.

;;; Code:
(require 'core/elpaca)
(require 'core/meow)

(define-leader meow-leader-snippet-map "s" "snippet"
  "Leader keymap for snippet-related keybindings.")

(use-package yasnippet
  :ensure t
  :diminish yas-minor-mode
  :hook
  (elpaca-after-init-hook . yas-global-mode)
  :bind
  (:map meow-leader-snippet-map
	("n" . yas-new-snippet)
	("i" . yas-insert-snippet)
	("e" . yas-visit-snippet-file)))

(use-package autoinsert
  ;; Easier to just always demand this, and it's fast.
  :demand t
  :hook (find-file-hook . auto-insert)
  :custom
  (auto-insert-query nil)
  (auto-insert-alist nil)
  :config
  ;; When we open a new file, automatically insert the file template
  (auto-insert-mode 1)

  (defun create-file-template (regex template mode)
    "Automatically insert the TEMPLATE snippet when REGEX match the file name."
    (add-to-list 'auto-insert-alist
		 `(,regex . [(lambda () (yas-expand-snippet (yas-lookup-snippet ,template ',mode)))]))))

(provide 'editor/snippets)
;;; snippets.el ends here
