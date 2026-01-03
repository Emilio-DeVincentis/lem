(in-package :lem-ai-chat)

(define-major-mode ai-chat-mode lem-markdown-mode:markdown-mode
    (:name "AI Chat"
     :keymap *ai-chat-mode-keymap*
     :syntax-table lem-markdown-mode::*markdown-syntax-table*)
  (setf (variable-value 'enable-undo :buffer) nil)
  (setf (variable-value 'line-wrap :buffer) t))

(define-major-mode ai-chat-prompt-mode lem-base-mode:fundamental-mode
    (:name "AI Chat Prompt"
     :keymap *ai-chat-prompt-mode-keymap*))

(define-key *ai-chat-prompt-mode-keymap* "Return" 'ai-chat-send)
(define-key *ai-chat-mode-keymap* "C-c C-r" 'ai-chat-fake-response)
(define-key *ai-chat-mode-keymap* "C-c C-d" 'ai-chat-diff-preview)
(define-key *ai-chat-mode-keymap* "C-c C-a" 'ai-chat-apply-change)

(defun find-history-buffer ()
  (get-buffer "*ai-chat-history*"))

(defun find-last-code-block (point)
  "Search backward from point to find the start of the last code block."
  (save-excursion
    (search-backward-regexp point "```")))

(define-command ai-chat-send () ()
  "Send the message in the prompt buffer to the chat history."
  (let* ((prompt-buffer (current-buffer))
         (history-buffer (find-history-buffer))
         (message (buffer-text prompt-buffer)))
    (erase-buffer prompt-buffer)
    (with-current-window (lem:frame-root-window)
      (switch-to-buffer history-buffer)
      (lem:buffer-end (lem:current-point))
      (insert-string (lem:current-point) (format nil "~%**You:**~%~%~a" message))
      (lem-markdown-mode::highlight-all))))

(define-command ai-chat-fake-response () ()
  "Simulate a response from the AI."
  (let ((history-buffer (find-history-buffer)))
    (with-current-window (lem:frame-root-window)
      (switch-to-buffer history-buffer)
      (lem:buffer-end (lem:current-point))
      (insert-string (lem:current-point)
                     (format nil "~%**AI:**~%~%Here's a code block:~%~%```lisp~%(defun hello ()~%  (print \"Hello, World!\"))~%```"))
      (lem-markdown-mode::highlight-all))))

(define-command ai-chat-diff-preview () ()
  "Show a diff of the last code block."
  (let* ((history-buffer (find-history-buffer))
         (point (buffer-point history-buffer))
         (start (find-last-code-block point)))
    (when start
      (with-point ((end start))
        (search-forward-regexp end "```")
        (let* ((code (points-to-string start end))
               (diff-buffer (make-buffer "*ai-chat-diff*" :temporary t)))
          (erase-buffer diff-buffer)
          (insert-string (buffer-point diff-buffer) code)
          (diff-buffers (lem:get-buffer "ai-chat.lisp") diff-buffer))))))

(define-command ai-chat-apply-change () ()
  "Apply the last code block to the target buffer."
  (let* ((history-buffer (find-history-buffer))
         (point (buffer-point history-buffer))
         (start (find-last-code-block point)))
    (when start
      (with-point ((end start))
        (search-forward-regexp end "```")
        (let* ((code (points-to-string start end))
               (target-buffer (lem:get-buffer "ai-chat.lisp")))
          (erase-buffer target-buffer)
          (insert-string (buffer-point target-buffer) code))))))

(define-command ai-chat () ()
  "Create a new AI chat buffer."
  (let* ((history-buffer (make-buffer "*ai-chat-history*" :temporary t))
         (prompt-buffer (make-buffer "*ai-chat-prompt*" :temporary t))
         (history-window (display-buffer history-buffer))
         (prompt-window (display-buffer prompt-buffer :height 5)))
    (setf (window-modeline-format prompt-window) " "
          (current-window) prompt-window)
    (switch-to-buffer history-buffer)
    (ai-chat-mode)
    (switch-to-buffer prompt-buffer)
    (ai-chat-prompt-mode)))
