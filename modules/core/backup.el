;;; core/backup ---  -*- lexical-binding: t; -*-

;;; Commentary:

;; Configure the Emacs backup system; see (info "(Emacs) Backup").

;;; Code:

(use-package files
  :ensure nil
  :demand t
  :preface
  (defconst backup-directory
    (expand-file-name "autosave" user-emacs-directory)
    "Directory containing autosave and backup files.")
  :custom
  (backup-directory-alist
   `((".*" . ,backup-directory))
   "Backup files go in the `autosave' directory.")
  (auto-save-file-name-transforms
   `((".*" ,(file-name-as-directory backup-directory) t))
   "Autosave files go in the `autosave' directory.")
  (backup-by-copying t "Backup via copying instead of renaming.")
  (delete-old-versions t "Silently delete excess backups.")
  (version-control t "Use numbered backup files.")
  (create-lockfiles nil "Do not create lockfiles."))

(provide 'core/backup)
;;; backup.el ends here
