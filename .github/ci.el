;;; ci.el --- CI file for mumacs -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(defun ci-print-diagnostic (string position _fill level)
  "Print a byte-compiler warning in a format that github actions can digest."
  ;; checkdoc-params: (string position fill level)
  (message ":%s file={%s},line={%s}::{%s}"
	   level
	   byte-compile-current-file
	   position
	   string))

(defun ci-byte-compile ()
  "Invoke the byte compiler on the modules directory for CI."
  (let* ((coding-system-for-read 'utf-8-unix)
         (coding-system-for-write 'utf-8)
         ;; We dont want to actually create any destination files.
	 ;; This actually doesn't matter for CI, but makes it possible to
	 ;; evaluate the usual CI.
         (byte-compile-dest-file-function #'ignore)
         ;; Disable error on warn: this messes with diagnostic levels.
         (byte-compile-error-on-warn nil)
         (byte-compile-log-buffer
          (generate-new-buffer " *dummy-byte-compile-log-buffer*"))
         (byte-compile-log-warning-function #'ci-print-diagnostic))
    ;; Invoke the byte compiler, and dump the output.
    (unwind-protect
        (byte-recompile-directory (concat user-emacs-directory "modules") 0 t)
      (ignore-errors
        (kill-buffer byte-compile-log-buffer)))))

(provide 'ci.el)
;;; ci.el ends here
