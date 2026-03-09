;;; core/elpaca --- Elpaca bootstrapping -*- no-byte-compile: t; lexical-binding: t; -*-

;;; Commentary:
;; This file includes the `elpaca' bootstrapping code, which
;; can be found at https://github.com/progfolio/elpaca?tab=readme-ov-file#installer

;;; Code:

;; Bootstrap `elpaca'.
;; This is taken from https://github.com/progfolio/elpaca?tab=readme-ov-file#installer.
(defvar elpaca-installer-version 0.12)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-sources-directory (expand-file-name "sources/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil :depth 1 :inherit ignore
                              :files (:defaults "elpaca-test.el" (:exclude "extensions"))
                              :build (:not elpaca-activate)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-sources-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (<= emacs-major-version 28) (require 'subr-x))
    (condition-case-unless-debug err
        (if-let* ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                  ((zerop (apply #'call-process `("git" nil ,buffer t "clone"
                                                  ,@(when-let* ((depth (plist-get order :depth)))
                                                      (list (format "--depth=%d" depth) "--no-single-branch"))
                                                  ,(plist-get order :repo) ,repo))))
                  ((zerop (call-process "git" nil buffer t "checkout"
                                        (or (plist-get order :ref) "--"))))
                  (emacs (concat invocation-directory invocation-name))
                  ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
                                        "--eval" "(byte-recompile-directory \".\" 0 'force)")))
                  ((require 'elpaca))
                  ((elpaca-generate-autoloads "elpaca" repo)))
            (progn (message "%s" (buffer-string)) (kill-buffer buffer))
          (error "%s" (with-current-buffer buffer (buffer-string))))
      ((error) (warn "%s" err) (delete-directory repo 'recursive))))
  (unless (require 'elpaca-autoloads nil t)
    (require 'elpaca)
    (elpaca-generate-autoloads "elpaca" repo)
    (let ((load-source-file-function nil)) (load "./elpaca-autoloads"))))
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))

;; Set up `use-package' support for `elpaca'.
;; There are a couple of options that must be set *before* `use-package' itself loads,
;; so we set them before we submit the order to `elpaca' to ensure this.

;; This is handy for debugging.
(setopt use-package-compute-statistics t)

;; This makes it easier to track down deferal mistakes; if we need to
;; `:demand', then we should call it out!
(setopt use-package-always-defer t)

;; Don't add `-hook' implicitly to hook names inside of a `:hook' block.
(setopt use-package-hook-name-suffix nil)

;; Allow `imenu' to pick up `use-package' blocks
(setopt use-package-enable-imenu-support t)

(elpaca elpaca-use-package
  ;; Enable `use-package' :ensure support for Elpaca.
  (elpaca-use-package-mode))

(provide 'core/elpaca)
;;; elpaca.el ends here
