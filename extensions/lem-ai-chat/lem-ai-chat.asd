(defsystem "lem-ai-chat"
  :description "AI Chat interface for Lem"
  :version "0.1.0"
  :depends-on ("lem" "lem-markdown-mode")
  :serial t
  :components ((:file "package")
               (:file "ai-chat")))
