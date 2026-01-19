;;; tools/gnus ---  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(use-package gnus
  :ensure nil
  :custom
  (gnus-select-method
   '(nnnil ""))
  (gnus-secondary-select-methods
   `((nnmaildir
      "personal"
      (directory "~/.local/share/mail/personal/")
      (gnus-search-engine
       gnus-search-notmuch
       (remove-prefix ,(expand-file-name "~/.local/share/mail/personal/"))
       (config-file ,(expand-file-name "~/.config/notmuch/default/config"))))))
  (gnus-asynchronous t)

  (gnus-summary-line-format
   (concat
    "%0{%U%R%z%}"
    "%3{│%}%1{%&user-date;%}%3{│%}" ;; date
    ;; "%ub:"          ;; indicate (+) if known (bbdb)
    "%4{%-20,20f%}" ;; name
    " "
    "%3{│%}"
    " "
    "%1{%B%}"
    "%s\n"))
  (gnus-thread-sort-functions
   '(gnus-thread-sort-by-most-recent-date
     (not gnus-thread-sort-by-number)))
  (gnus-show-threads t)
  (gnus-sum-thread-tree-false-root nil)
  (gnus-sum-thread-tree-root nil)
  (gnus-sum-thread-tree-indent " ")
  (gnus-sum-thread-tree-vertical        "│")
  (gnus-sum-thread-tree-leaf-with-other "├─► ")
  (gnus-sum-thread-tree-single-leaf     "╰─► "))

(provide 'tools/gnus)
;;; gnus.el ends here
