;;; core/meow ---  -*- lexical-binding: t; no-byte-compile: t; -*-

;;; Commentary:

;;; Code:
(require 'f)

(defun find-emacs-module ()
  "Open an Emacs configuration module."
  (interactive)
  (let ((default-directory (f-join user-emacs-directory "modules/")))
    (call-interactively 'find-file)))

(defun find-package-module ()
  "Open a third-party Emacs module."
  (interactive)
  (let ((default-directory (f-join user-emacs-directory "straight/repos/")))
    (call-interactively 'find-file)))

(defun open-init-file ()
  "Open the initialization file."
  (interactive)
  (find-file (f-join user-emacs-directory "init.el")))

;; Modal editing
(use-package meow
  ;; No way we can defer loading this; we really do need keystrokes to work ASAP.
  :demand t
  :custom
  (meow-use-clipboard t "Use the system clipboard.")
  (meow-cheatsheet-layout meow-cheatsheet-layout-qwerty)
  (meow-keypad-ctrl-meta-prefix ?M "Do not use `SPC g' as a shortcut for `C-M-'.")
  (meow-keypad-self-insert-undefined nil)
  :config
  (require 'meow)

  ;; By default, `meow' uses `mode-specific-map' as the leader keymap.
  ;; This leads to problematic situations when we try to set up a doom-like
  ;; leader system, so we create a fresh keymap for the leader and use that instead.
  (setq meow-leader-keymap (define-keymap))
  (setf (alist-get 'leader meow-keymap-alist) meow-leader-keymap)

  ;; `meow' comes with a "keypad" system that is close to `god-mode',
  ;; where pressing a leader key (in our case, `SPC') allows you to
  ;; chord keys without pressing modifiers.
  (defconst meow-file-keymap
    (define-keymap
      "r" '("open recent" . recentf-open)
      "f" '("find file" . find-file)
      "x" '("delete file" . delete-file)
      "s" '("save file" . save-buffer)
      "m" '("find emacs module" . find-emacs-module)
      "i" '("open init file" . open-init-file)))

  (defconst meow-buffer-keymap
    (define-keymap
      "b" '("find buffer" . switch-to-buffer)
      "d" '("kill buffer" . kill-current-buffer)))

  (meow-define-keys 'leader
    `("f" . ("file" . ,meow-file-keymap))
    `("b" . ("buffer" . ,meow-buffer-keymap)))

  (meow-motion-overwrite-define-key
   '("j" . meow-next)
   '("k" . meow-prev)
   '("<escape>" . ignore)
   '("'" . meow-temp-normal))

  (meow-define-keys 'leader
    `("SPC" . ("execute command" . execute-extended-command))
    `("," . ("find buffer" . switch-to-buffer))
    `("." . ("find file" . find-file)))

  (meow-define-keys 'normal
    '("0" . meow-expand-0)
    '("9" . meow-expand-9)
    '("8" . meow-expand-8)
    '("7" . meow-expand-7)
    '("6" . meow-expand-6)
    '("5" . meow-expand-5)
    '("4" . meow-expand-4)
    '("3" . meow-expand-3)
    '("2" . meow-expand-2)
    '("1" . meow-expand-1)
    '("-" . negative-argument)
    '(";" . meow-reverse)
    '("," . meow-inner-of-thing)
    '("." . meow-bounds-of-thing)
    '("C-j" . meow-beginning-of-thing)
    '("C-k" . meow-end-of-thing)
    '("a" . meow-append)
    '("A" . meow-open-below)
    '("b" . meow-back-word)
    '("B" . meow-back-symbol)
    '("c" . meow-change)
    '("d" . meow-delete)
    '("D" . meow-backward-delete)
    '("e" . meow-next-word)
    '("E" . meow-next-symbol)
    '("g" . meow-cancel-selection)
    '("G" . meow-grab)
    '("h" . meow-left)
    '("H" . meow-left-expand)
    '("i" . meow-insert)
    '("I" . meow-open-above)
    '("j" . meow-next)
    '("J" . meow-next-expand)
    '("k" . meow-prev)
    '("K" . meow-prev-expand)
    '("l" . meow-right)
    '("L" . meow-right-expand)
    '("m" . meow-join)
    '("n" . meow-search)
    '("o" . meow-block)
    '("O" . meow-to-block)
    '("p" . meow-yank)
    '("Q" . meow-goto-line)
    '("r" . meow-replace)
    '("R" . meow-swap-grab)
    '("s" . meow-kill)
    '("t" . meow-till)
    '("u" . meow-undo)
    '("C-r" . undo-redo)
    '("U" . meow-undo-in-selection)
    '("v" . meow-visit)
    '("w" . meow-mark-word)
    '("W" . meow-mark-symbol)
    '("x" . meow-line)
    '("X" . meow-goto-line)
    '("y" . meow-save)
    '("Y" . meow-sync-grab)
    '("z" . meow-pop-selection)
    '("'" . repeat)
    '("=" . meow-indent)
    '(">" . meow-find-ref)
    '(":" . meow-comment)
    '("<" . meow-pop-marker)
    '("%" . recenter)
    '("<escape>" . ignore))
  (meow-global-mode))

(use-package hippie-exp
  :straight nil
  :bind (("M-p" . hippie-expand))
  :custom
  ((hippie-expand-verbose nil)))

;; Load `which-key' after `meow' to ensure that it gets
;; used for `meow-keymap'.
(use-package which-key
  :after meow
  :diminish which-key-mode
  :demand t
  :config
  (which-key-mode))

;; Used for wrapping selection in parens, braces, quotes, etc.
(use-package surround
  :demand t
  :after meow
  :config
  (meow-define-keys 'normal
    '("(" . surround-insert)
    '(")" . surround-delete)
    '("{" . surround-change)))

;; Window management
(use-package ace-window
  :demand t
  :custom
  (aw-dispatch-always t)
  :config
  (meow-define-keys
      'leader
    '("w" . ("window" . ace-window)))
  ;; `SPC w w' for quick window changes
  (add-to-list 'aw-dispatch-alist '(?a delete-other-wi))
  (add-to-list 'aw-dispatch-alist '(?w aw-flip-window))
  (add-to-list 'aw-dispatch-alist '(?l windmove-right))
  (add-to-list 'aw-dispatch-alist '(?h windmove-left))
  (add-to-list 'aw-dispatch-alist '(?j windmove-down))
  (add-to-list 'aw-dispatch-alist '(?k windmove-up)))

(provide 'core/meow)
;;; meow.el ends here
