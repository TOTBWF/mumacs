;;; editor/company ---  -*- lexical-binding: t; -*-

;;; Commentary:
;; This file contains the initialization code for
;; `company-mode', and also sets up some custom
;; `use-package' integrations that makes adding
;; backends a bit easier.

;;; Code:
(require 'core/straight)

(use-package company
  :diminish company-mode
  :commands company-mode
  :defer t
  :bind (:map company-mode-map
	      ("C-<tab>" . company-other-backend)))

;;;###autoload
(defun company-use-package--company-normalize (name keyword args)
  "Normalizer for `:company' in `use-package' forms.
NAME, KEYWORD, and ARGS are explained in the `use-package' documentation."
  (use-package-as-one (symbol-name keyword) args
    (lambda (label arg)
      ;; We want to ensure that each argument is of the form
      ;; `some-mode' or `(some-mode . company-handler)'.
      (unless (or (use-package-non-nil-symbolp arg) (consp arg))
	(use-package-error
	 (concat label
		 " a <symbol>"
		 " or (<symbol or list of symbols> . <symbol or function>)"
		 " or a list of these")))
      (use-package-normalize-pairs
       (lambda (key)
	 (or (use-package-non-nil-symbolp key)
	     (and (consp key)
		  (use-package-non-nil-symbolp (car key))
		  )))
       #'use-package-recognize-function
       name label args t))))

;;;###autoload
(defun company-use-package--company-handler (name _keyword args rest state)
  "Handler for `:company' in `use-package' forms.
NAME, KEYWORD, ARGS, REST, and STATE are explained in
`use-package' documentation."
  (use-package-concat
   (use-package-process-keywords name rest state)
   (mapcan
    (lambda (def)
      (let ((modes (car def))
	    (backend (cdr def)))
	(when backend
	  (mapcar
	   (lambda (mode)
	     `(add-hook
	       (quote ,(intern (concat (symbol-name mode) use-package-hook-name-suffix)))
	       (lambda ()
		 (setq-local company-backends
			     (add-to-list 'company-backends (quote ,backend))))))
	   (use-package-hook-handler-normalize-mode-symbols modes)))))
    (use-package-normalize-commands args))))

(defalias 'use-package-normalize/:company 'company-use-package--company-normalize)
(defalias 'use-package-handler/:company 'company-use-package--company-handler)

;; Add a `:company' use-package keyword for easier configuration.
;; We make sure to place our keyword at the end!
(add-to-list 'use-package-keywords ':company t)

(provide 'editor/company)
;;; company.el ends here
