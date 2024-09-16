;;; editor/snippets ---  -*- lexical-binding: t; -*-

;;; Commentary:
;; I use `yasnippet' for all my snippeting needs, and I use it pretty heavily!
;;
;; I also use a combination of `autoinsert' and `yasnippet' to insert file headers
;; automatically.

;;; Code:
(require 'core/straight)
(require 'core/meow)

(use-package yasnippet
  :diminish yas-minor-mode
  :hook
  ;; FIXME: should be smarter about deferring this. However, benchmarking
  ;; shows that this is negligible.
  (after-init . yas-global-mode)
  :config
  (defconst meow-yas-keymap
    (define-keymap
      "n" '("new snippet" . yas-new-snippet)
      "i" '("insert snippet" . yas-insert-snippet)
      "e" '("edit snippet" . yas-visit-snippet-file)))
  (meow-define-keys 'leader `("s" . ("snippet" . ,meow-yas-keymap))))

(use-package autoinsert
  ;; Easier to just always demand this, and it's fast.
  :demand t
  :hook (find-file . auto-insert)
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

(defun camel-case (s)
  "Convert a snake_case string S into camelCase."
  (let* ((upcased (mapconcat 's-capitalize (s-split "_" s 'omit-nulls) ""))
         (head (substring upcased 0 1))
         (tail (substring upcased 1)))
    (concat (s-downcase head) tail)))


(provide 'editor/snippets)
;;; snippets.el ends here
