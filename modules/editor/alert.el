;;; editor/alert ---  -*- lexical-binding: t; -*-

;;; Commentary:

;; Notifications and alerts.

;;; Code:
(require 'core/elpaca)

;; Ensure that `posframe' is properly loaded, and set up autoloads
;; for all the functions that we will need.
(use-package posframe
  :autoload
  posframe-show
  posframe-delete
  posframe-poshandler-frame-top-center)

;; We use `alert.el', along with a custom alert style based off posframes.
(use-package alert
  :ensure t
  :demand t
  :autoload
  alert
  alert-define-style
  :functions
  alert-posframe-buffer-name
  alert-posframe-format-message
  alert-posframe-message-face
  alert-dismiss
  :custom
  (alert-default-style 'posframe)
  :advice
  ;; HACK: `alert.el' dismisses notifications whenever emacs
  ;; every time a command is executed. This ends up dismissing
  ;; most notifications *immediately*
  (alert-remove-on-command :override ignore)
  :config
  (defun alert-posframe-buffer-name (info)
    "Compute a buffer name for alert INFO."
    (concat "*alert " (plist-get info :title) "*"))

  (defun alert-posframe-format-message (info)
    "Format alert INFO for display in a posframe."
    (concat (plist-get info :title) "\n\n" (plist-get info :message)))

  (defun alert-posframe-message-face (info)
    "Get the face used to display alert INFO based off the severity.
This is customizable through `alert-severity-faces'."
    (cdr (assq (plist-get info :severity) alert-severity-faces)))

  (alert-define-style
   'posframe
   :title "Display a posframe"
   :notifier
   (lambda (info)
     (posframe-show
      (alert-posframe-buffer-name info)
      :string (alert-posframe-format-message info)
      :poshandler #'posframe-poshandler-frame-top-center
      :timeout nil
      :initialize
      (lambda ()
	(face-remap-add-relative 'default (alert-posframe-message-face info)))))
   :remover
   (lambda (info)
     (when (or (plist-get info :never-persist) (not (plist-get info :persistent)))
       (posframe-delete (alert-posframe-buffer-name info))))))

(provide 'editor/alert)
;;; alert.el ends here
