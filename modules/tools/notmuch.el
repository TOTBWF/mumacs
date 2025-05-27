;;; tools/notmuch ---  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:
(require 'core/meow)

(defvar meow-notmuch-search-map (make-keymap))

(with-eval-after-load 'meow
  (meow-define-state notmuch-search
    "Meow state for interacting with the `notmuch' search buffer."
    :lighter "[E]"
    :keymap meow-notmuch-search-map)

  (add-to-list 'meow-mode-state-list '(notmuch-search-mode . notmuch-search))
  (add-to-list 'meow-mode-state-list '(notmuch-tree-mode . notmuch-search))
  (add-to-list 'meow-mode-state-list '(notmuch-show-mode . motion)))

(use-package notmuch
  :ensure t
  :commands notmuch
  :custom
  (notmuch-show-logo nil)
  (notmuch-search-oldest-first nil)
  (notmuch-tagging-keys
   '(("a" ("+archive" "-unread" "-inbox") "Archive")
     ("d" ("+deleted" "-inbox" "-unread") "Delete")
     ("r" ("+recipt" "-inbox" "-unread") "Recipt")
     ("s" ("+spam" "-inbox" "-unread") "Spam")
     ("t" ("+todo" "-inbox") "Todo")))
  (notmuch-archive-tags '("-inbox" "-unread" "+archive"))
  (notmuch-always-prompt-for-sender t)
  (notmuch-saved-searches
   '((:name "inbox" :query "tag:inbox" :key "i")
     (:name "unread" :query "tag:unread" :key "u")
     (:name "todo" :query "tag:todo" :key "t")
     (:name "flagged" :query "tag:flagged" :key "f")
     (:name "drafts" :query "tag:draft" :key "d")
     (:name "all mail" :query "*" :key "a")))
  :bind
  (:map ctl-x-map
	("m" . notmuch-mua-new-mail))
  (:map meow-notmuch-search-map
	("j" . meow-next)
	("k" . meow-prev)
	("t" . notmuch-tag-jump)
    	("SPC" . meow-keypad)))

(use-package ol-notmuch
  :ensure t
  :after notmuch org
  :demand t)

(use-package sendmail
  :ensure nil
  :custom
  (send-mail-function 'sendmail-send-it)
  (sendmail-program "/run/current-system/sw/bin/msmtp")
  (mail-specify-envelope-from t)
  (mail-envelope-from 'header)
  (message-sendmail-envelope-from 'header))


;; IMAP idle notifier.
;; (require 'alert)
(require 'imap)

(defun imap-idle-filter (on-notify proc string)
  ; checkdoc-params: (on-notify proc string)
  "IMAP IDLE process filter."
  (when (buffer-name (process-buffer proc))
    (with-current-buffer (process-buffer proc)
      (goto-char (point-max))
      (insert string)
      (imap-log string)
      (let (end)
	(goto-char (point-min))
	(while (setq end (imap-find-next-line))
	  (save-restriction
	    (narrow-to-region (point-min) end)
	    (delete-char (- (length imap-server-eol)))
	    (goto-char (point-min))
	    (unwind-protect
		(cond ((eq imap-state 'initial)
		       (imap-parse-greeting))
		      ((or (eq imap-state 'auth)
			   (eq imap-state 'nonauth)
			   (eq imap-state 'selected)
			   (eq imap-state 'examine))
		       (imap-parse-response)
		       (funcall on-notify))
		      (t
		       (message "Unknown state %s in arrival filter"
				imap-state)))
	      (delete-region (point-min) (point-max)))))))))

(cl-defun imap-start-idle (&key server port stream user mailbox on-notify)
  "Start an IMAP IDLE session with the host SERVER on port PORT.

STREAM indicates the stream type to use, see `imap-streams' for the available
stream types.

Attempts to authenticate with the username USER by using `auth-source-search'.
If the authentication fails, an error is thrown.  Once authenticated, calls
select on the mailbox MAILBOX, and then starts an IMAP IDLE.

Every time the server responds to with a command during the IDLE, the
function ON-NOTIFY is called with no arguments."
  (if-let* ((auth (car (auth-source-search :host server :user user :require '(:secret) :max 1))))
   (with-current-buffer (imap-open server port stream 'login)
    ;; `imap-authenticate' is overkill for us.
    (imap-ok-p
     (imap-send-command-wait
      (format "LOGIN \"%s\" \"%s\""
	      (imap-quote-specials user)
	      (imap-quote-specials (funcall (plist-get auth :secret))))))
    (setq imap-state 'auth)
    (imap-mailbox-select mailbox 'read-only)
    ;; Initiate the IDLE, and replace the `imap-arrival-filter'
    ;; process filter with one that invokes the ON-NOTIFY argument.
    (imap-send-command "IDLE")
    (set-process-filter (get-buffer-process (current-buffer))
			(apply-partially #'imap-idle-filter on-notify)))
   (error "imap: Authenticating to `%s' using failed" server)))

;; (imap-start-idle
;;  :server "imap.gmail.com"
;;  :user "reedmullanix@gmail.com"
;;  :mailbox "INBOX"
;;  :stream 'tls
;;  :on-notify (lambda () (alert "You've got mail!" :title "reedmullanix@gmail.com" :severity 'high)))

;; (imap-start-idle
;;  :server "localhost"
;;  :user "mullanir@mcmaster.ca"
;;  :port 1143
;;  :stream 'network
;;  :mailbox "INBOX"
;;  :on-notify (lambda () (alert "You've got mail!" :title "mullanir@mcmaster.ca" :severity 'high)))


(provide 'tools/notmuch)
;;; notmuch.el ends here
