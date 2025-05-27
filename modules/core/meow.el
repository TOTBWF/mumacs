;;; core/meow ---  -*- lexical-binding: t; no-byte-compile: t; -*-

;;; Commentary:

;;; Code:
(require 'core/elpaca)

(defun find-emacs-module ()
  "Open an Emacs configuration module."
  (interactive)
  (let ((default-directory (f-join user-emacs-directory "modules/")))
    (call-interactively 'find-file)))

(defun find-package-module ()
  "Open a third-party Emacs module."
  (interactive)
  (let ((default-directory (f-join user-emacs-directory "elpaca/repos/")))
    (call-interactively 'find-file)))

(defun open-init-file ()
  "Open the initialization file."
  (interactive)
  (find-file (f-join user-emacs-directory "init.el")))

;; [NOTE: Meow leader keymaps]
;; By default, `meow' uses `mode-specific-map' as the leader keymap
;; (EG: the keymap used for `C-c'). This means that if we try and add
;; bindings like `SPC g g' for magit, and a mode then binds `C-c g', then
;; our binding get clobbered. To avoid this, we are going to bind a new
;; keymap called `meow-leader-map' that we are going to use for our leader.
;;
;; This needs to be done outside of the `use-package' statement for `meow'.
;; Later on, we are going to add entries to this keymap via a
;; combination of `with-eval-after-load' and `meow-define-keys'.
;; If we don't declare the keymap outside the `use-package' form, then
;; the `with-eval-after-load' will run *before* the `:config' block, which
;; in turn will clobber all of our bindings!
(defvar meow-leader-map (define-keymap)
  "Keymap to use for the SPC leader key.
This should be modified via `define-leader'.")

(defmacro define-leader (name key description &optional docstring)
  "Define NAME as a leader keymap for `meow', and bind it to KEY in `meow-leader-map'.
DESCRIPTION is a string used to describe the prefix.
DOCSTRING is an optional docstring to use for the keymap."
  (declare
   (ftype (function (symbol string string &optional string) nil))
   (doc-string 4)
   (indent 3))
  (cl-check-type name symbol)
  (cl-check-type description string)
  (cl-check-type docstring (or null string))
  `(progn
     ;; Use `defvar' over `defconst' so that re-evaluating the macro doesn't
     ;; clobber our bindings.
     (defvar ,name (make-sparse-keymap) ,docstring)
     ;; We don't actually need to wait until `meow' loads: just insert
     ;; into the keymap directly.
     (define-key meow-leader-map (kbd ,key) (cons ,description ,name))))

(defmacro define-keypad (name char &optional docstring)
  "Define a new `meow' keypad entry with NAME for CHAR with an optional DOCSTRING."
  (declare
   (ftype (function (symbol character &optional string) nil))
   (doc-string 3)
   (indent 2))
  (cl-check-type name symbol)
  (cl-check-type char character)
  (cl-check-type docstring (or null string))
  `(progn
     ;; Use `defvar' over `defconst' so that re-evaluating the macro doesn't
     ;; clobber our bindings.
     (defvar ,name (make-sparse-keymap))
     ;; We can avoid a somewhat annoying call to `kbd' by just setting the
     ;; `C-' event modifier directly.
     (define-key (current-global-map) ,(vector (event-apply-modifier char 'control 26 "C-")) ,name)
     ;; Have to wait until `meow' finishes loading before we add our map to the keypad
     ;; translation layer.
     (with-eval-after-load 'meow
       (if (boundp 'meow-keypad-start-keys)
	   (add-to-list 'meow-keypad-start-keys (quote ,(cons char char)))
	 (warn "define-keypad: `meow-keypad-start-keys' not bound.")))))


;; Modal editing
(use-package meow
  ;; No way we can defer loading this; we really do need keystrokes to work ASAP.
  :ensure t
  :demand t
  :custom
  (meow-use-clipboard t "Use the system clipboard.")
  (meow-cheatsheet-layout meow-cheatsheet-layout-qwerty)
  (meow-keypad-ctrl-meta-prefix ?M "Do not use `SPC g' as a shortcut for `C-M-'.")
  (meow-keypad-self-insert-undefined nil)
  :config
  (require 'meow)

  ;; See [NOTE: Meow leader keymaps].
  (setf (alist-get 'leader meow-keymap-alist) meow-leader-map)

  ;; `meow' comes with a "keypad" system that is close to `god-mode',
  ;; where pressing a leader key (in our case, `SPC') allows you to
  ;; chord keys without pressing modifiers.
  (defconst meow-file-map
    (define-keymap
      "r" '("open recent" . recentf-open)
      "f" '("find file" . find-file)
      "x" '("delete file" . delete-file)
      "s" '("save file" . save-buffer)
      "m" '("find emacs module" . find-emacs-module)
      "i" '("open init file" . open-init-file)))

  (defconst meow-buffer-map
    (define-keymap
      "b" '("find buffer" . switch-to-buffer)
      "d" '("kill buffer" . kill-current-buffer)
      "x" '("scratch buffer" . scratch-buffer)))

  (meow-define-keys 'leader
    `("f" . ("file" . ,meow-file-map))
    `("b" . ("buffer" . ,meow-buffer-map)))

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
  :ensure nil
  :bind (("M-p" . hippie-expand))
  :custom
  ((hippie-expand-verbose nil)))

;; Load `which-key' after `meow' to ensure that it gets
;; used for `meow-keymap'.
(use-package which-key
  :ensure t
  :diminish which-key-mode
  :demand t
  :config
  (which-key-mode))

;; Used for wrapping selection in parens, braces, quotes, etc.
(use-package surround
  :ensure t
  :demand t
  :after meow
  :config
  (meow-define-keys 'normal
    '("(" . surround-insert)
    '(")" . surround-delete)
    '("{" . surround-change)))

;; Window management
(use-package ace-window
  :ensure t
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
