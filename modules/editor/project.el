;;; editor/project ---  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:
(require 'core/meow)

(use-package project
  :straight nil
  ;; Don't bother defering; gets quite fiddly with autoloading,
  ;; and benchmarking shows that this is fast to load.
  :demand t
  :config
  ;; We need to place the entire prefix map into the meow leader keymap,
  ;; which is not an operations supported by `:bind-keymap'. Luckily loading
  ;; `project' is pretty snappy, so we don't need to stress too hard about deferring.
  (meow-define-keys 'leader `("p" . ("project" . ,project-prefix-map))))

(provide 'editor/project)
;;; project.el ends here
