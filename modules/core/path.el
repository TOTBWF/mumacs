;;; core/path ---  -*- lexical-binding: t; -*-

;;; Commentary:
;; I know that `exec-path-from-shell' is quite cursed, but unfortunately
;; seems to be the most reliable way to correctly populate the `process-environment'
;; with the correct values when not launched from a shell.  In the future, I could
;; look at caching some of these values, but this works for now.

;;; Code:

(use-package exec-path-from-shell
  :config
  (after-init-hook . exec-path-from-shell-initialize)
  :custom
  ;; If we want `compile' to play nicely with `envrc-mode', then
  ;; we need to make sure to grab a bunch of extra shell variables.
  ;; See https://github.com/purcell/envrc/issues/92.
  (exec-path-from-shell-variables
   '("SSH_AUTH_SOCK"
     "SSH_AGENT_PID"
     "XDG_DATA_DIRS"
     "XDG_CONFIG_DIRS"
     "NIX_USER_PROFILE_DIR"
     "NIX_SSL_CERT_FILE"
     "NIX_PROFILES"
     "NIX_PATH"
     "PATH"
     "MANPATH")))

(provide 'core/path)
;;; path.el ends here
