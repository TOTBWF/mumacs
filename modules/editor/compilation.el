;;; editor/compilation ---  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

;; Replace ANSI color escape codes with faces.
(use-package ansi-color
  :ensure t
  :hook
  (compilation-filter-hook . ansi-color-compilation-filter)
  :custom
  (ansi-color-for-compilation-mode t))

(use-package compile
  :ensure nil
  :preface
  (defun compilation-start--use-pipe (fn &rest args)
    "Advice to ensure that `compilation-start' uses a pipe rather than a pty
for the compilation command. This increases performance on OSX by a factor of 10,
as the default pty size is a pitiful 1024 bytes."
    (let ((process-connection-type nil))
      (apply fn args)))
  :advice
  (compilation-start :around compilation-start--use-pipe))

(provide 'editor/compilation)
;;; compilation.el ends here
