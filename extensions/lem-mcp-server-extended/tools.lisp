(in-package :lem-mcp-server-extended)

(defconstant +request-cancelled+ -32800)

(defmacro define-secure-mcp-tool (name (&rest args) (&key description input-schema) &body body)
  "Define and register a secure MCP tool that requires user confirmation."
  `(define-mcp-tool ,name (,@args) (:description ,description :input-schema ,input-schema)
     (if (lem:prompt-for-y-or-n-p (format nil "MCP tool '~A' wants to run. Allow?" ,name))
         (progn ,@body)
         (mcp-error +request-cancelled+ "User cancelled the execution"))))

(define-mcp-tool "file_list" (path)
  (:description "List files in a directory"
   :input-schema (("type" . "object")
                  ("properties" . (("path" . (("type" . "string")
                                              ("description" . "Path to the directory (optional, defaults to project root)")))))
                  ("required" . ())))
  (let* ((base-path (if path
                        (truename path)
                        (lem-core/commands/project:find-root (lem:buffer-directory)))))
    (unless (uiop:directory-exists-p base-path)
      (mcp-error +invalid-params+
                 (format nil "Directory not found: ~A" base-path)))
    (with-output-to-string (s)
      (yason:encode
       (mapcar (lambda (p)
                 (let ((is-dir (uiop:directory-exists-p p)))
                   `(("name" . ,(file-namestring p))
                     ("path" . ,(namestring p))
                     ("isDirectory" . ,(if is-dir :true :false)))))
               (uiop:directory-files base-path))
       s))))

(define-secure-mcp-tool "shell_execute" (command)
  (:description "Execute a shell command"
   :input-schema (("type" . "object")
                  ("properties" . (("command" . (("type" . "string")
                                                 ("description" . "The shell command to execute")))))
                  ("required" . ("command"))))
  (handler-case
      (uiop:run-program command :output :string :error-output :output)
    (error (e)
      (mcp-error +server-error+
                 (format nil "Command execution failed: ~A" e)))))
