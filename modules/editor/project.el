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
  (meow-define-keys 'leader `("p" . ("project" . ,project-prefix-map)))

  ;; Some useful helper functions.
  (defun project-make-relative-to-root (filename &optional project)
    "Convert FILENAME to be relative to the root of a `project.el' PROJECT.  Defaults to `project-current'"
    (file-relative-name filename (project-root (or project (project-current))))))

(provide 'editor/project)
;;; project.el ends here
