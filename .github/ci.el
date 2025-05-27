;;; ci.el --- CI file for mumacs -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(defun ci-print-diagnostic (string position _fill level)
  ;; checkdoc-params: (string position fill level)
  "Print a byte-compiler warning in a format that github actions can digest.

See https://docs.github.com/en/actions/writing-workflows/choosing-what-your-workflow-does/workflow-commands-for-github-actions#setting-an-error-message.
Note that this will create a buffer that visits the source of the diagnostic."
  (with-current-buffer (find-file-noselect byte-compile-current-file t)
    (goto-char position)
    (message ":%s file=%s,line=%s,col=%s::%s"
             level
             (file-relative-name byte-compile-current-file user-emacs-directory)
             (line-number-at-pos)
             (1+ (- (point) (line-beginning-position)))
             string)))

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
