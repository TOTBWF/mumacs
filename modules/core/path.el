;;; core/path ---  -*- lexical-binding: t; -*-

;;; Commentary:
;; `exec-path-from-shell' is somewhat cursed, so we roll our own solution.
;; Instead of trying to gather our path from the shell, we explicitly
;; set the `PATH' variable inside of Emacs via `add-to-path'.

;;; Code:

(defun add-to-path (path)
  "Add PATH to the variable `exec-path' and update $PATH.
This is used in place of `exec-path-from-shell' to avoid having
to start up a shell process, and is also more consistent."
  (let ((expanded-path (expand-file-name path)))
    (add-to-list 'exec-path expanded-path)
    (setenv "PATH" (concat expanded-path ":" (getenv "PATH")))))

(add-to-path "/usr/bin")
(add-to-path "/usr/local/bin")
(add-to-path "/run/current-system/sw/bin")
(add-to-path "~/.local/bin")

(provide 'core/path)
;;; path.el ends here
