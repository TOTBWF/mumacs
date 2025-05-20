;;; core/straight --- Boostrapping code for straight.el  -*- lexical-binding: t; -*-

;;; Commentary:

;; Bootstrap `straight.el' and `use-package`, and configure some core
;; features that should happen early in the boostrapping process.

;;; Code:

;; Bootstrapping code for `straight.el'; taken from https://github.com/radian-software/straight.el.
(defvar bootstrap-version)
(defconst straight-use-package-by-default t)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(require 'straight)

;; Load up `use-package', so we can declaratively manage packages.
;; Note that we want to use HEAD for `use-package', as we want
;; `which-key' integration in the `:bind' form.
(straight-use-package
 '(use-package
    :type git
    :host github
    :repo "jwiegley/use-package"))

(require 'use-package)

(provide 'core/straight)
;;; straight.el ends here
