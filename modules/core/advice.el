;;; core/advice ---  -*- lexical-binding: t; -*-

;;; Commentary:
;; This file adds a `:advice' keyword to `use-package'.

;;; Code:

(require 'core/elpaca)

(defun use-package-normalize-advice (_name keyword args)
  ;; checkdoc-params: (keyword)
  "Ensure ARGS is a valid advice form for the `use-package' `:advice' keyword.
See Info node `(use-package)Creating an extension'."
  (cl-loop
   for (sym how fn props) in args
   if (and (symbolp sym)
	   (assq how advice--how-alist)
	   (use-package-recognize-function fn))
   collect (list sym how fn props) into advice
   else do
   (use-package-error
    (format "%s wants arguments acceptable to `advice-add', or a list of such values"
	    keyword))
   finally return advice))

(defun use-package-autoloads-advice (_name _keyword args)
  ;; checkdoc-params: (name args)
  "Autoloads handler for the `use-package' `:advice' keyword.
See Info node `(use-package)Creating an extension'."
  (mapcar (lambda (arg) (cons (caddr arg) 'command)) args))

(defun use-package-handler-advice (name _keyword args rest state)
  ;; checkdoc-params: (name args rest state)
  "Handler for the `use-package' `:advice' keyword.
See Info node `(use-package)Creating an extension'."
  (use-package-concat
   (use-package-process-keywords name rest state)
   `(,@(mapcar (pcase-lambda (`(,sym ,how ,fn ,props)) `(advice-add (quote ,sym) ,how (function ,fn) ,props)) args))))

(defalias 'use-package-normalize/:advice 'use-package-normalize-advice)
(defalias 'use-package-autoloads/:advice 'use-package-autoloads-advice)
(defalias 'use-package-handler/:advice 'use-package-handler-advice)

;; Make sure to place the `:advice' keywords before `:commands', as we are setting up autoloads.
(push :advice (nthcdr (use-package-keyword-index :commands) use-package-keywords))


(provide 'core/advice)
;;; advice.el ends here
