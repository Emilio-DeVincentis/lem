(in-package :lem-context-manager)

(defun get-cursor-position-context ()
  "Get the current cursor position as a formatted string."
  (let* ((buffer (current-buffer))
         (point (buffer-point buffer))
         (filename (buffer-filename buffer)))
    (if filename
        (format nil "File: ~A, Line: ~A, Column: ~A"
                filename
                (line-number-at-point point)
                (point-charpos point))
        "Cursor is in a non-file buffer.")))

(defun get-recent-files-context ()
  "Get the last 5 visited files as a formatted string."
  (let ((recent-files (lem-core/commands/file:recent-files)))
    (when recent-files
      (format nil "Recent Files:~%~{  - ~A~%~}" (subseq recent-files 0 (min 5 (length recent-files)))))))

(defun get-lsp-diagnostics-context ()
  "Get the current LSP diagnostics as a formatted string."
  ;; NOTE: This uses an internal function from lem-lsp-mode.
  ;; It would be better to have a public API for this.
  (let ((diagnostics (lem-lsp-mode::buffer-diagnostics (current-buffer))))
    (when diagnostics
      (format nil "LSP Diagnostics:~%~{  - ~A~%~}"
              (mapcar #'lem-lsp-mode::diagnostic-message diagnostics)))))

(defun format-java-symbol (symbol indent)
  "Format a single Java symbol with indentation."
  (with-output-to-string (s)
    (dotimes (i indent) (princ "  " s))
    (format s "- ~A (~A)"
            (lem-lsp-mode::document-symbol-name symbol)
            (lem-lsp-mode::symbol-kind-to-string (lem-lsp-mode::document-symbol-kind symbol)))
    (dolist (child (lem-lsp-mode::document-symbol-children symbol))
      (princ (format-java-symbol child (1+ indent)) s))))

(defun get-java-class-structure-context ()
  "Get the Java class structure as a formatted string."
  (when (equal (major-mode) 'lem-java-mode:java-mode)
    (let ((symbols (lem-lsp-mode::text-document/document-symbol (current-buffer))))
      (when symbols
        (with-output-to-string (s)
          (princ "Java Class Structure:" s)
          (dolist (symbol symbols)
            (princ (format-java-symbol symbol 1) s)))))))

(defun get-context-prompt ()
  "Gather all context information and format it into a single string."
  (with-output-to-string (s)
    (let ((cursor-context (get-cursor-position-context))
          (recent-files-context (get-recent-files-context))
          (lsp-diagnostics-context (get-lsp-diagnostics-context))
          (java-class-structure-context (get-java-class-structure-context)))
      (when cursor-context
        (princ cursor-context s)
        (terpri s))
      (when recent-files-context
        (princ recent-files-context s)
        (terpri s))
      (when lsp-diagnostics-context
        (princ lsp-diagnostics-context s)
        (terpri s))
      (when java-class-structure-context
        (princ java-class-structure-context s)
        (terpri s)))))

(define-command context-manager-display-context () ()
  "Gather all context information and display it in a new buffer."
  (let ((context-prompt (get-context-prompt)))
    (with-pop-up-typeout-window (s (make-buffer "*context-prompt*") :erase t)
      (princ context-prompt s))))
