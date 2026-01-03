(defsystem "lem-context-manager"
  :description "A context management system for AI agents in Lem."
  :version "0.1.0"
  :author "Jules"
  :license "MIT"
  :depends-on ("lem"
               "lem-lsp-mode"
               "lem-java-mode")
  :serial t
  :components ((:file "package")
               (:file "context-manager")))
