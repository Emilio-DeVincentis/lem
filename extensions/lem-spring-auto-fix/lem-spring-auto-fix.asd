(defsystem "lem-spring-auto-fix"
  :description "An agentic function to automatically fix Spring Boot compilation errors."
  :version "0.1.0"
  :author "Jules"
  :license "MIT"
  :depends-on ("lem"
               "lem-terminal")
  :serial t
  :components ((:file "package")
               (:file "spring-auto-fix")))
