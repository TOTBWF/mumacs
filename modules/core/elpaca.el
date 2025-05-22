;;; core/elpaca --- DESCRIPTION -*- no-byte-compile: t; lexical-binding: t; -*-

;;; Commentary:
;; This file includes the `elpaca' bootstrapping code, which
;; can be found at https://github.com/progfolio/elpaca?tab=readme-ov-file#installer

;;; Code:

(defun emacs-repository-get-revision-date (revision &optional dir)
  "Try to get the commit time of REVISION from the Emacs sources.

Returns an Emacs timestemp.

If DIR is non-nil, use that directory insteaad of `source-directory'."
  (with-temp-buffer
    (let ((default-directory (or dir source-directory)))
      ;; We make sure to wrap this whole block in `with-demoted-errors'
      ;; to avoid blowing up Emacs entirely if we can't get the revision
      ;; date for some reason.
      (with-demoted-errors "Error running git show -s --format=%%cI: %S"
	(with-temp-buffer
	  (let ((exit-code (call-process "git" nil t nil "show" "-s" "--format=%cI" revision))
		(contents (buffer-substring-no-properties (point-min) (- (point-max) 1))))
	    (if (zerop exit-code)
		(encode-time (iso8601-parse contents))
	      (error "%s" contents))))))))


;; Set `emacs-build-time' explicitly if it looks like we were built from source.
;; We can use `emacs-repository-version' to determine if Emacs was built from
;; a repository checkout.
(when (and emacs-repository-version (not emacs-build-time))
  ;; If it looks like we were built from source, we can try to work
  ;; backwards from the git hash to determine a date.
  ;; Note that this requires the `source-directory' to be set
  ;; to an actual git repository: this can be the case with
  ;; some nix-built versions of Emacs. In these situation,
  ;; users are recommended to set `source-directory' in `custom.el'.
  (defvar elpaca-core-date (emacs-repository-get-revision-date emacs-repository-version)))

(defvar elpaca-installer-version 0.11)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order
  '(elpaca :repo "https://github.com/progfolio/elpaca.git"
           :ref nil :depth 1 :inherit ignore
           :files (:defaults "elpaca-test.el" (:exclude "extensions"))
           :build (:not elpaca--activate-package)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
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

(elpaca elpaca-use-package
  ;; Enable `use-package' :ensure support for Elpaca.
  (elpaca-use-package-mode))

(provide 'core/elpaca)
;;; elpaca.el ends here
