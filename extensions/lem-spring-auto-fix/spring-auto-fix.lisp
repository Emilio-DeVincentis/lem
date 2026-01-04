(in-package :lem-spring-auto-fix)

(defvar *auto-fix-timer* nil)
(defvar *auto-fix-retries* 0)
(defvar *auto-fix-limit* 5)
(defvar *auto-fix-build-command* "./mvnw compile")

(defun run-build ()
  "Run the build command and check the result."
  (let ((terminal-buffer (lem-terminal:term-execute *auto-fix-build-command* :name "*spring-auto-fix-terminal*")))
    (lem-terminal:term-send-input terminal-buffer "
")
    (start-timer (make-idle-timer (lambda () (check-build-status terminal-buffer))) 1000 :repeat nil)))

(defun check-build-status (terminal-buffer)
  "Check the build status and extract the stack trace on failure."
  (let* ((output (buffer-text terminal-buffer))
         (build-failed (search "BUILD FAILURE" output)))
    (if build-failed
        (progn
          (message "Build failed. Trying to fix...")
          (let ((stacktrace (extract-stacktrace output)))
            (send-to-llm stacktrace (current-buffer))))
        (progn
          (message "Build successful!")
          (stop-timer *auto-fix-timer*)))))

(defun extract-stacktrace (output)
  "Extract the stacktrace from the build output."
  ;; This is a simplified implementation. A real implementation would need to
  ;; parse the output more carefully.
  (let ((start (search "[ERROR] " output)))
    (when start
      (subseq output start))))

(defun send-to-llm (stacktrace buffer)
  "Send the stacktrace and source code to the LLM and get a fix."
  (declare (ignore stacktrace buffer))
  ;; For now, this is a stub that returns a hardcoded fix.
  (let ((fix "
<<<<<<< SEARCH
    public String greet() {
        return \"Hello, World!\";
    }
=======
    public String greet() {
        return \"Hello, World!\";
    }

    public String greet2() {
        return \"Hello, World2!\";
    }
>>>>>>> REPLACE
"))
    (apply-fix fix (current-buffer))))

(defun apply-fix (fix buffer)
  "Apply the fix to the buffer."
  (ppcre:register-groups-bind (search-content replace-content)
      ("(?s)<<<<<<< SEARCH\
(.*?)=======\
(.*?)>>>>>>> REPLACE" fix)
    (switch-to-buffer buffer)
    (lem:replace-string (buffer-point buffer) search-content replace-content)
    (if (< *auto-fix-retries* *auto-fix-limit*)
        (progn
          (incf *auto-fix-retries*)
          (start-auto-fix-loop))
        (message "Auto-fix limit reached."))))

(define-command spring-auto-fix () ()
  "Start the Spring auto-fix agent."
  (setf *auto-fix-retries* 0)
  (start-auto-fix-loop))

(defun start-auto-fix-loop ()
  "Start the auto-fix loop."
  (when *auto-fix-timer*
    (stop-timer *auto-fix-timer*))
  (setf *auto-fix-timer*
        (start-timer (make-idle-timer 'run-build) 100 :repeat nil)))
