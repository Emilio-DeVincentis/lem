(defpackage #:lem-mcp-server-extended
  (:use #:cl #:lem)
  (:import-from #:lem-mcp-server
                #:define-mcp-tool
                #:mcp-error
                #:+invalid-params+
                #:+server-error+)
  (:export #:define-secure-mcp-tool))
