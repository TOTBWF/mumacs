;;; tools/notmuch ---  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(setq meow-notmuch-search-map (make-keymap))
(meow-define-state notmuch-search
  "Meow state for interacting with the `notmuch' search buffer."
  :lighter "[E]"
  :keymap meow-notmuch-search-map)

(add-to-list 'meow-mode-state-list '(notmuch-search-mode . notmuch-search))

(use-package notmuch
  :commands notmuch
  :functions notmuch-delete notmuch-recipt
  :custom
  (notmuch-search-oldest-first nil)
  (notmuch-tagging-keys
   '(("d" ("+deleted" "-new" "-unread") "Delete")
     ("r" ("+recipt" "-new" "-unread")) "Recipt"))
  (notmuch-archive-tags '("-new" "-unread" "+archive"))
  (notmuch-always-prompt-for-sender t)
  :config
  (defun notmuch-delete ()
    "Flag the current message as deleted."
    (interactive)
    (notmuch-search-tag '("+deleted" "-new" "-unread")))

  (defun notmuch-recipt ()
    "Flag the current message as a recipt."
    (interactive)
    (notmuch-search-tag '("+recipt" "-new" "-unread")))

  :bind
  (:map ctl-x-map
	("m" . notmuch-mua-new-mail))
  (:map meow-notmuch-search-map
	("j" . meow-next)
	("k" . meow-prev)
	("t" . notmuch-tag-jump)
	("d" . notmuch-delete)
	("r" . notmuch-recipt)
	("SPC" . meow-keypad)))

(use-package sendmail
  :straight nil
  :custom
  (send-mail-function sendmail-send-it)
  (sendmail-program "/run/current-system/sw/bin/msmtp")
  (mail-specify-envelope-from t)
  (mail-envelope-from 'header)
  (message-sendmail-envelope-from 'header))

(provide 'tools/notmuch)
;;; notmuch.el ends here
