;;; narya-mode ---  -*- lexical-binding: t; -*-

;;; Commentary:

;; `narya-mode' is a major mode for editing narya files.
;; The current mode is quite spartan, and only provides basic
;; syntax highlighting and interaction commands.

;;; Code:
(require 'compile)

(defgroup narya nil "Narya." :prefix 'narya :group 'languages)

(defcustom narya-command "narya"
  "The command used to invoke `narya'."
  :group 'narya
  :type 'string
  :tag "Narya Command"
  :options '("narya"))

(setq narya-command "~/Documents/Projects/narya/_build/install/default/bin/narya")

(defcustom narya-command-flags '("-dtt" "-verbose" "-discreteness")
  "Flags to pass to `narya'."
  :group 'narya
  :type (list 'string)
  :tag "Narya Flags")

(defconst narya-definition-keywords
  '("axiom" "def" "and")
  "Keywords that proceed definitions.")

(defconst narya-command-keywords
  '("echo" "notation" "section")
  "Command keywords.")

(defconst narya-builtin-keywords
  '("Type" "Id" "refl" "sym" "Gel" "ungel")
  "Builtin keywords.")

(defvar narya--dynamic-terms '()
  "List of dynamically identified terms for highlighting.")

(defun narya--update-dynamic-terms ()
  "Parse the buffer for user-defined constants.
This function mutates `narya--dynamic-terms'."
  (save-excursion
    (goto-char (point-min))
    (let ((terms '()))
      (setq narya--dynamic-terms '())
      (while (re-search-forward "\\(axiom\\|def\\) \\(\\w+\\(?:_\\w+\\)*\\)\\(?:\\s-+\\(?:\\(.*?\\)\\s-*:\\)?\\)\\s-*" nil t)
        (let ((const-name (match-string 2)))
          (unless (member const-name terms)
            (push const-name terms))))
      ;; Remove duplicates and update the global list
      (setq narya--dynamic-terms (delete-dups terms)))))

(defun narya--apply-dynamic-highlighting ()
  "Apply dynamic highlighting to user-defined constants."
  (font-lock-add-keywords nil `((,(regexp-opt narya--dynamic-terms 'words) . 'font-lock-function-name-face)) t))

(defun narya-dynamic-highlight-hook (_start _end _old-len)
  "Update dynamic highlighting info.
Intended to be used with `after-change-functions'."
  (narya--update-dynamic-terms)
  (narya--apply-dynamic-highlighting))

(defconst narya-mode-font-lock-keywords
  `(
    ;; Declaration commands
    (,(regexp-opt narya-definition-keywords 'words) 0 'font-lock-keyword-face)
    ;; Other commands
    (,(regexp-opt narya-command-keywords '()) 0 'font-lock-preprocessor-face)
    ;; Numbers
    (,(rx word-start (?  "-") (+ digit)) 0 'font-lock-number-face)
    ;; Builtins
    (,(regexp-opt narya-builtin-keywords) 0 'font-lock-builtin-face)))

(defvar narya-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?\` "< 23b" table)
    (modify-syntax-entry ?\n "> b" table)
    (modify-syntax-entry ?\{ "(}1nb" table)
    (modify-syntax-entry ?\} "){4nb" table)
    table)
  "Syntax table for `narya-mode'.")

(defconst narya-compilation-error-regex-alist
  '(("^ ￫ \\(error\\)?\\(warning\\)?\\(info\\)?\\[\\([[:alnum:]]*\\)\\]\n\\(\\(?:.*\n\\)*?\\)\n"
     ;; We don't care about trying to extract source locations.
     nil nil nil 2 4))
  "Regular expressions used to match `narya' compilation errors.
See the documentation of `compilation-error-regexp-alist' for details.")

(define-compilation-mode narya-compilation-mode "Narya"
  "Narya-specific `compilation-mode' derivative."
  (setq-local compilation-error-regexp-alist narya-compilation-error-regex-alist))

(defconst narya--compilation-buffer-name
  "*narya*"
  "The name to use for narya compilation buffers.")

(defun narya--compilation-buffer-name-function (_mode)
  "Compute a buffer name for the `narya-mode' compilation buffer."
  narya--compilation-buffer-name)

(defun narya-compile-buffer ()
  "Load the current buffer into `narya'."
  (interactive)
  (let ((filename (buffer-file-name)))
    (unless filename
      (error "Buffer has no file name"))
    (when (buffer-modified-p) (save-buffer))
    (let* ((dir (file-name-directory filename))
	   (file (file-name-nondirectory filename))
	   (opts (mapconcat 'identity narya-command-flags " "))
	   (command (concat narya-command " " opts " " file))
	   ;; Dynamically bind variables for `compilation-start'.
	   (compilation-environment '("TERM=xterm-color"))
	   (compilation-buffer-name-function 'narya--compilation-buffer-name-function)
	   (default-directory dir))
      (compilation-start command 'narya-compilation-mode))))

(define-derived-mode narya-mode prog-mode "Narya"
  "Major mode for editing `narya' files.

Shortcuts for interacting with `narya-mode':
\\{narya-mode-map}"
  :group 'narya
  :syntax-table narya-syntax-table

  ;; Comment styles; used by `comment-*' functions.
  (setq-local comment-start "` ")
  (setq-local block-comment-start "{`")
  (setq-local block-comment-end "`}")

  ;; Do not use tabs.
  (setq-local indent-tabs-mode nil)

  ;; Font locking
  (setq font-lock-defaults '((narya-mode-font-lock-keywords) nil nil))
  (add-hook 'after-change-functions 'narya-dynamic-highlight-hook)

  (define-key narya-mode-map (kbd "C-c C-c") 'narya-compile-buffer))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.ny\\'" . narya-mode))


(provide 'lang/narya)
;;; narya-mode.el ends here
