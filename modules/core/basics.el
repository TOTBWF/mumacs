;;; core/basics --- Boostrapping code  -*- lexical-binding: t; -*-

;;; Commentary:

;; Configure some core features that should happen early in the boostrapping
;; process.

;;; Code:

(require 'core/elpaca)


;; We want to load this relatively early so that we can restart emacs nicely
;; even if we make a mistake in our config.
(use-package restart-emacs
  :ensure t
  :demand t)

;; `diminish' lets us easily hide active modes from the modeline.
;; This seems like a bit of an odd place to load this, but
;; we use it absolutely everywhere, so we want it right
;; out the gate.
(use-package diminish
  :ensure t
  :demand t
  :functions diminish
  :config
  (diminish 'global-whitespace-mode)
  (diminish 'auto-revert-mode)
  (diminish 'eldoc-mode)
  (diminish 'abbrev-mode))

;; We also want to configure native compilation to use a decent optimization level.
;; See (info "(elisp) Native Compilation") for more details.
(use-package comp
  :ensure nil
  :demand t
  :if (native-comp-available-p)
  :custom
  (native-comp-speed 2 "Use the highest safe optimization level for native compilation."))

;;; Core packages:

;; These packages are extremely common, so we load them first ourselves
;; so we can use them too!

;; `dash.el' provides a more consistent API for manipulating lists; see (info "(dash) Top") for docs.
(use-package dash
  :ensure t
  :demand t)

;; `f.el' provides a more consistent API for filepath manipulation.
(use-package f
  :ensure t
  :demand t)

;; `s.el' provides a more consistent API for string manipulation.
(use-package s
  :ensure t
  :demand t)

(defun randomize-region (beg end)
  (interactive "r")
  (replace-region-contents
   (min beg end)
   (max beg end)
   (lambda ()
     (concat (shuffle-list (string-to-list (buffer-string)))))))

(defun shuffle-list (list)
  "Randomly permute the elements of LIST.
All permutations equally likely."
  (let ((i 0)
  j
  temp
  (len (length list)))
    (while (< i len)
      (setq j (+ i (random (- len i))))
      (setq temp (nth i list))
      (setcar (nthcdr i list) (nth j list))
      (setcar (nthcdr j list) temp)
      (setq i (1+ i))))
  list)

(defun whitespace-compress-region (start end)
  (interactive "@r")
  (replace-regexp-in-region (rx (+ " ")) " " start end))

(provide 'core/basics)
;;; basics.el ends here
